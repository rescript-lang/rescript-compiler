(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)





module E = Js_exp_make


let handle_external 
    (module_name : Lam_external_def.external_module_name option) = 
  match module_name with 
  | Some {bundle ; bind_name} -> 
    let id  = 
      match bind_name with 
      | None -> 
        Lam_compile_env.add_js_module bundle , bundle
      | Some bind_name -> 
        Lam_compile_env.add_js_module 
          ~id:(Ext_ident.create_js_module bind_name) bundle,
        bundle
    in Some id 
  | None -> None 

type typ = Ast_core_type.t

let ocaml_to_js last
    (js_splice : bool)
    ((label : string), (ty : typ))
    (arg : J.expression) 
  : E.t list = 
  if last && js_splice 
  then if Ast_core_type.is_array ty then 
      match arg with 
      | {expression_desc = Array (ls,_mutable_flag) } -> 
        ls (* Invariant : Array encoding *)
      | _ -> 
        assert false  
        (* TODO: fix splice, 
           we need a static guarantee that it is static array construct
           otherwise, we should provide a good error message here
        *)
    else  assert false
  else if Ast_core_type.is_unit ty then [] (* ignore unit *)
  else 
    (* for 
       [x:t] -> "x"
       [?x:t] -> "?x"
    *)
    match Type_util.label_name label with 
    | `Optional label -> [Js_of_lam_option.get_default_undefined arg]
    | _ ->  [arg]  
          


let translate_ffi  loc (ffi : Lam_external_def.ffi ) prim_name
    (cxt  : Lam_compile_defs.cxt)
    ( ty :typ ) 
    (args : J.expression list) = 
    match ffi with 
    | Obj_create -> 
      let _return_type, arg_types = Ast_core_type.list_of_arrow ty in
      let key loc label = 
        Js_op.Key (Lam_methname.translate ?loc  label) in 
      let kvs : J.property_map = 
        Ext_list.filter_map2 
          (fun (label, (ty : typ)) 
            ( arg : J.expression) -> 
            if Ast_core_type.is_unit ty then None 
            else 
            match Type_util.label_name label with 
            | `Label label -> 
              Some (key loc label, arg)
            | `Optional label -> 
              begin 
                match arg.expression_desc with 
                | Array ([x;y], _mutable_flag)  ->
                  Some (key loc label, y) (*Invrariant: optional encoding*)
                | Number _ -> (*Invariant: None encoding*)
                  None
                | _ ->  (* FIXME: avoid duplicate evlauation of [arg] when it
                                              is not a variable [Var ]
                                              can only bd detected at runtime thing *)
                  Some ( key loc label, 
                         Js_of_lam_option.get_default_undefined arg)
              end) arg_types args 
          (* (Ext_list.exclude_tail arg_types) (Ext_list.exclude_tail args) *)
      in 
      E.obj kvs 

    | Js_call{ external_module_name = module_name; 
               txt = { name = fn; splice = js_splice ; 
                       qualifiers;
                     }} -> 
        let _return_type, arg_types = Ast_core_type.list_of_arrow ty in
        let args = 
          Ext_list.flat_map2_last (ocaml_to_js js_splice) arg_types args  in 
        let qualifiers =  List.rev qualifiers in
        let fn =  
          match handle_external module_name with 
          | Some (id,_) -> 
            (* FIXME: need add dependency here
                it's an external call 
            *)
            List.fold_left E.dot (E.var id) (qualifiers @ [ fn])
          | None -> 
            begin 
              match  qualifiers @ [fn] with 
              | y::ys  -> 
                List.fold_left E.dot (E.js_var y) ys
              | _ -> assert false
            end
        in
        if Ast_core_type.is_unit _return_type then
          E.seq (E.call ~info:{arity=Full; call_info = Call_na} fn args) (E.unit)
        else             
          E.call ~info:{arity=Full; call_info = Call_na} fn args

    | Js_global_as_var module_name -> 
      begin match handle_external (Some module_name) with 
        | Some (id, name) -> 
          E.external_var_dot id name None
        | None -> assert false 
      end
    | Js_new { external_module_name = module_name; 
               txt = { name = fn};
             } -> 
      let _return_type, arg_types = Ast_core_type.list_of_arrow ty in
      let args = 
        Ext_list.flat_map2_last (ocaml_to_js false) arg_types args  in 
      let fn =  
        match handle_external module_name with 
        | Some (id,name) ->  
          E.external_var_dot id name (Some fn)

        | None -> 
          (** TODO: check, no [@@bs.module], 
              assume it's global *)
          E.js_var fn

      in
      (* This has some side effect, it will 
         mark its identifier (If it has) as an object,
         ATTENTION: 
         order also matters here, since we mark its jsobject property, 
         it  will affect the code gen later
         TODO: we should propagate this property 
         as much as we can(in alias table)
      *)
      (
        match cxt.st with 
        | Declare (_, id) | Assign id  ->
          (* Format.fprintf Format.err_formatter "%a@."Ident.print  id; *)
          Ext_ident.make_js_object id 
        | EffectCall | NeedValue -> ()
      );
      E.new_ fn args



    | Js_global {name; external_module_name} -> 

      (* TODO #11
         1. check args -- error checking 
         2. support [@@bs.scope "window"]
         we need know whether we should call [add_js_module] or not 
      *)
      begin match name, handle_external external_module_name with 
        | "true", None -> E.js_bool true
        | "false", None -> E.js_bool false
        | "null", None -> E.nil 
        | "undefined", None -> E.undefined
        | _, Some(id,mod_name)
          -> E.external_var_dot id mod_name (Some name)
        | _, None -> 

          E.var (Ext_ident.create_js name)
      end
    | Js_send {splice  = js_splice ; name } -> 
      begin match args  with
        | self :: args -> 
          let [@warning"-8"] (_return_type, self_type::arg_types )
            = Ast_core_type.list_of_arrow ty in
          let args = Ext_list.flat_map2_last (ocaml_to_js js_splice) arg_types args in
          E.call ~info:{arity=Full; call_info = Call_na}  (E.dot self name) args
        | _ -> 
          Location.raise_errorf ?loc "Ill defined attribute"
      end
    | Js_get {name} -> 
      begin match args with 
      | [obj] ->
        E.dot obj name        
      | _ ->  
          Location.raise_errorf ?loc "Ill defined attribute"
        (* There is a minor drawback here, only when it is called, 
           the error will be triggered *)
      end  
    | Js_set {name} -> 
      begin match args with 
      | [obj; v] -> 
        E.assign (E.dot obj name) v         
      | _ -> 
        Location.raise_errorf ?loc "Ill defined attribute"
      end
    | Js_get_index 
      -> 
      begin match args with
        | [obj; v ] -> 
          Js_array.ref_array obj v
        | _ -> Location.raise_errorf ?loc "Ill defined attribute"
      end
    | Js_set_index 
      -> 
      begin match args with 
      | [obj; v ; value] -> 
        Js_array.set_array obj v value
      | _ -> Location.raise_errorf ?loc "Ill defined attribute"
      end
    


let translate cxt 
    ({prim_name ; } as prim 
     : Lam_external_def.prim) args  = 
  match Lam_external_def.handle_attributes prim with 
  | Normal -> Lam_dispatch_primitive.translate prim_name args 
  | Bs (ty, loc, ffi) -> 
    let () = Lam_external_def.check_ffi ?loc ffi in
    translate_ffi loc ffi prim_name cxt ty args 



(* TODO:
  Also need to mark that CamlPrimtivie is used and
  add such dependency in
  module loader 
*)
