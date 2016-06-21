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


(** 
   [@@bs.module "react"]
   [@@bs.module "react"]
   ---
   [@@bs.module "@" "react"]
   [@@bs.module "@" "react"]
   
   They should have the same module name 

   TODO: we should emit an warning if we bind 
   two external files to the same module name
*)

let handle_external (module_name : Lam_external_def.external_module_name option) = 
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


let handle_attributes ({prim_attributes ; prim_name} as _prim  : Lam_external_def.prim )
  : Location.t option * Lam_external_def.ffi  = 
  let qualifiers = ref [] in
  let call_name = ref None in
  let external_module_name  = ref None in
  let is_obj =  ref false in
  let js_val = ref `None in
  let js_val_of_module = ref `None in 
  let js_send = ref `None in
  let js_set = ref `None in
  let js_get = ref `None in
  let js_set_index = ref false in 
  let js_get_index = ref false in

  let js_splice = ref false in
  let start_loc : Location.t option ref = ref None in
  let finish_loc = ref None in
  let js_new = ref None in
  let () = 
    prim_attributes |> List.iter
      (fun ((( x : string Asttypes.loc ), pay_load) : Parsetree.attribute) -> 
         (if !start_loc = None  then 
            start_loc := Some x.loc 
         ); 
         (finish_loc := Some x.loc);
         match x.txt with  (* TODO: Check duplicate attributes *)
         | "bs.val"
           (* can be generalized into 
              {[
                [@@bs.val]
              ]}
              and combined with 
              {[
                [@@bs.value] [@@bs.module]
              ]}
           *)
           -> 
           begin  match Ast_payload.is_single_string pay_load with
             | Some name -> 
               js_val := `Value name 
             | None -> 
               js_val := `Value prim_name
                (* we can report error here ... *)
           end
         | "bs.val_of_module" 
           (* {[ [@@bs.val_of_module]]}
           *)
           -> 
           js_val_of_module := 
             `Value (Lam_external_def.{bundle = prim_name ; bind_name = Ast_payload.is_single_string pay_load})
         |"bs.splice"
           -> 
           js_splice := true

         |"bs.send" 
           ->
           begin match Ast_payload.is_single_string pay_load with 
             | Some name -> js_send := `Value name
             | None -> js_send := `Value prim_name
           end
         | "bs.set"
           ->
           begin match Ast_payload.is_single_string pay_load with
             | Some name -> js_set := `Value name
             | None -> js_set := `Value prim_name
           end
         | "bs.get"
           ->
           begin match Ast_payload.is_single_string pay_load with
             | Some name -> js_get := `Value name
             | None -> js_get := `Value prim_name
           end

         | "bs.call"
           (*TODO: check duplicate attributes, at least we should give a warning
             [@@bs.call "xx"] [@@bs.call]
           *)
           ->
           begin match Ast_payload.is_single_string pay_load with 
             | Some name -> call_name :=  Some (x.loc, name)
             | None -> call_name := Some(x.loc, prim_name)
           end
         | "bs.module" -> 
           begin match Ast_payload.is_string_or_strings pay_load with 
             | `Single name ->
               external_module_name:= Some (Lam_external_def.{ bundle =  name; bind_name = None})
             | `Some [bundle;bind_name] -> 
               external_module_name := 
                 Some (Lam_external_def.{bundle ; bind_name = Some bind_name})
             | `Some _ -> ()
             | `None -> () (* should emit a warning instead *)
           end

         | "bs.new" -> 
           begin match Ast_payload.is_single_string pay_load with 
             | Some x -> js_new := Some x 
             | None -> js_new := Some prim_name
           end
         | "bs.set_index" 
           -> js_set_index := true
         | "bs.get_index"
           -> js_get_index := true
         |"bs.obj"
           -> 
           is_obj := true
         | _ ->  () (* ignore *)
      ) in
  let loc : Location.t option  = 
    match !start_loc, !finish_loc  with
    | None, None -> None 
    | Some {loc_start;_}, Some{loc_end; _} -> Some {loc_start; loc_end; loc_ghost = false}
    | _ -> assert false in
    loc, 
    if !is_obj then Obj_create 
    else if !js_get_index then
      Js_get_index
    else if !js_set_index then 
      Js_set_index
    else 
    begin match !js_val_of_module with 
    | `Value v -> Js_global_as_var v 
    | `None -> 
      begin match !call_name, !js_val, !js_send, !js_new, !js_set, !js_get  with 
      | Some (_,fn),
        `None, `None, _, `None, `None -> 
          Js_call { txt = { splice = !js_splice; qualifiers = !qualifiers; name = fn};
                  external_module_name = !external_module_name}
      | None, `Value name, `None ,_, `None, `None  ->  
          Js_global {name = name; external_module_name = !external_module_name}
      | None, `None, `Value name, _, `None, `None   -> 
          Js_send {splice = !js_splice; name }
      | None, `None, `None, Some name, `None, `None  -> 
          Js_new { txt = { name  };
                   external_module_name = ! external_module_name}
      |  None, `None, `None, None, `Value name, `None 
        -> Js_set { name}
      |  None, `None, `None, None, `None,  `Value name
        -> Js_get {name} (* TODO, we should also have index *)
      |  None, `None, `None, None, `None, `None -> Normal 
      | _ -> 
          Location.raise_errorf ?loc "Ill defined attribute"
      end
    end
    (* Given label, type and the argument --> encode it into 
       javascript meaningful value 
       -- check whether splice or not for the last element
     *)
    (*
      special treatment to None for [bs.call] as well
      None --> null or undefined 
      Some -> original value
      unit -->
     *)

let ocaml_to_js last
    (js_splice : bool)
    ((label : string), (ty : Types.type_expr))
    (arg : J.expression) 
  : E.t list = 
  if last && js_splice 
  then 
    match ty with 
    | { desc = Tconstr(p,_,_) } when Path.same p Predef.path_array -> 
      begin 
        match arg with 
        | {expression_desc = Array (ls,_mutable_flag) } -> 
          ls (* Invariant : Array encoding *)
        | _ -> 
          assert false  (* TODO: fix splice *)
      end
    | _ -> assert false
  else
    match ty, Type_util.label_name label with 
    | { desc = Tconstr(p,_, _)}, _ when Path.same p Predef.path_unit -> []
    | { desc = Tconstr(p,_,_) }, _ when Path.same p Predef.path_bool -> 
      begin 
        match arg.expression_desc with 
        | Number (Int {i = 0l; _} 
        (* | Float {f = "0."} This should not happen *)
       ) ->  [E.caml_false]
        | Number _ -> [E.caml_true]
        | _ -> [E.econd arg E.caml_true E.caml_false]
      end

    | _, `Optional label -> 
      begin 
        match (arg.expression_desc) with 
        | Array ([x;y],_mutable_flag)  ->
          [ y] (*Invrariant: optional encoding*)
        | Number _ -> (*Invariant: None encoding*)
          [ E.nil ] 
        (* when no argumet is supplied, [undefined] 
           if we detect that all rest arguments are [null], 
           we can remove them
        *)
        | _ ->  (* FIXME: avoid duplicate evlauation of [arg] when it
                   is not a variable
                *)
          (* | Var _  ->  *)
          (* can only bd detected at runtime thing *)
          (* (E.bin EqEqEq (E.typeof arg) *)
          (*   (E.str "number")) *)

          [E.econd arg
             (Js_of_lam_option.get arg )
             E.undefined
             ]

      end
    | _ ->  [arg]  
          

let translate 
    (cxt  : Lam_compile_defs.cxt)
    ({prim_attributes; prim_ty } as prim
     : Types.type_expr option Primitive.description) 
    (args : J.expression list) = 
  begin 
    let loc, ffi = handle_attributes prim in
    let () = Lam_external_def.check_ffi ?loc ffi in 
    match ffi with 
    | Obj_create -> 
      begin 
        match prim_ty with 
        | Some ty -> 
          let _return_type, arg_types = Type_util.list_of_arrow ty in
          let key loc label = 
            Js_op.Key (Lam_methname.translate ?loc  label) in 
          let kvs : J.property_map = 
            Ext_list.filter_map2 (fun (label, (ty : Types.type_expr)) 
                                   ( arg : J.expression) -> 
                match ty.desc, Type_util.label_name label with 
                | Tconstr(p,_, _), _ when Path.same p Predef.path_unit 
                  -> None
                | Tconstr(p,_,_), `Label label  when Path.same p Predef.path_bool 
                  -> 
                  begin 
                    match arg.expression_desc with 
                    | Number ((* Float { f = "0."}| *) Int { i = 0l;_}) 
                      -> 
                      Some (key loc label ,E.caml_false)
                    | Number _ -> 
                      Some (key loc label,E.caml_true)
                    | _ -> Some (key loc label, (E.econd arg E.caml_true E.caml_false))
                  end

                | _, `Label label -> 
                  Some (key loc label, arg)
                | _, `Optional label -> 
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
                             E.econd arg
                               (* (E.bin EqEqEq (E.typeof arg) *)
                               (*   (E.str "number")) *)

                               (Js_of_lam_option.get arg)
                               E.undefined
                           )
                  end)                   
              arg_types args 
              (* (Ext_list.exclude_tail arg_types) (Ext_list.exclude_tail args) *)
          in 
          E.obj kvs 
        | None -> assert false 

      end
    | Js_call{ external_module_name = module_name; 
               txt = { name = fn; splice = js_splice ; 
                       qualifiers;
                     }} -> 
      begin 
        match prim_ty with 
        | Some ty -> 
          let _return_type, arg_types = Type_util.list_of_arrow ty in
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
          if Type_util.is_unit _return_type then
            E.seq (E.call ~info:{arity=Full; call_info = Call_na} fn args) (E.unit)
          else             
            E.call ~info:{arity=Full; call_info = Call_na} fn args
        | None -> assert false 
      end
    | Js_global_as_var module_name -> 
      begin match handle_external (Some module_name) with 
        | Some (id, name) -> 
          E.external_var_dot id name None
        | None -> assert false 
      end
    | Js_new { external_module_name = module_name; 
               txt = { name = fn};
             } -> 
      begin 
        match prim_ty with 
        | Some ty -> 
          let _return_type, arg_types = Type_util.list_of_arrow ty in
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
        | None -> assert false 
      end

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
      begin 
        match args , prim_ty with
        | self :: args, Some ty -> 
          let [@warning"-8"] (_return_type, self_type::arg_types )
            = Type_util.list_of_arrow ty in
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
    | Normal -> Lam_dispatch_primitive.query prim args 


  end

(* TODO:
  Also need to mark that CamlPrimtivie is used and
  add such dependency in
  module loader 
*)
