(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Translation from typed abstract syntax to lambda terms,
   for the module language *)



open Misc
open Asttypes
open Longident
open Path
open Types
open Typedtree
open Lambda

open Translcore


type error =
  Circular_dependency of Ident.t
| Conflicting_inline_attributes

exception Error of Location.t * error

(* Keep track of the root path (from the root of the namespace to the
   currently compiled module expression).  Useful for naming extensions. *)

let global_path glob = Some(Pident glob)
let is_top rootpath = 
  match rootpath with 
  | Some (Pident _ ) -> true
  | _ -> false 

let functor_path path param =
  match path with
    None -> None
  | Some p -> Some(Papply(p, Pident param))
let field_path path field =
  match path with
    None -> None
  | Some p -> Some(Pdot(p, Ident.name field, Path.nopos))

(* Compile type extensions *)

let transl_type_extension env rootpath tyext body =
  List.fold_right
    (fun ext body ->
      let lam =
        transl_extension_constructor env (field_path rootpath ext.ext_id) ext
      in
      Llet(Strict, Pgenval, ext.ext_id, lam, body))
    tyext.tyext_constructors
    body

(* Compile a coercion *)

let rec apply_coercion loc strict restr arg =
  match restr with
    Tcoerce_none ->
      arg
  | Tcoerce_structure(pos_cc_list, id_pos_list, runtime_fields) ->
      assert (List.length runtime_fields = List.length pos_cc_list);
      let names = Array.of_list runtime_fields in 
      name_lambda strict arg (fun id ->
        let get_field_i i pos = Lprim(Pfield (pos, Fld_module {name = names.(i)}),[Lvar id], loc) in
        let get_field_name name pos =
            Lprim (Pfield (pos, Fld_module {name}), [Lvar id], loc) in 
        let lam =
          Lprim(Pmakeblock(Lambda.Blk_module runtime_fields),
                List.mapi (fun i x -> apply_coercion_field loc (get_field_i i) x) pos_cc_list,
                loc)
        in
        wrap_id_pos_list loc id_pos_list get_field_name lam)
  | Tcoerce_functor(cc_arg, cc_res) ->
      let param = Ident.create "funarg" in
      let carg = apply_coercion loc Alias cc_arg (Lvar param) in
      apply_coercion_result loc strict arg [param] [carg] cc_res
  | Tcoerce_primitive { pc_loc; pc_desc; pc_env; pc_type; } ->
      transl_primitive pc_loc pc_desc pc_env pc_type None
  | Tcoerce_alias (path, cc) ->
      name_lambda strict arg
        (fun _ -> apply_coercion loc Alias cc (transl_normal_path path))

and apply_coercion_field loc get_field (pos, cc) =
  apply_coercion loc Alias cc (get_field pos)

and apply_coercion_result loc strict funct params args cc_res =
  match cc_res with
  | Tcoerce_functor(cc_arg, cc_res) ->
    let param = Ident.create "funarg" in
    let arg = apply_coercion loc Alias cc_arg (Lvar param) in
    apply_coercion_result loc strict funct
      (param :: params) (arg :: args) cc_res
  | _ ->
    name_lambda strict funct (fun id ->
      Lfunction{ params = List.rev params;
                attr = { default_function_attribute with
                         is_a_functor = true;
                         stub = true; };
                loc = loc;
                body = apply_coercion
                         loc Strict cc_res
                         (Lapply{
                                 ap_loc=loc;
                                 ap_func=Lvar id;
                                 ap_args=List.rev args;
                                 ap_inlined=Default_inline;
                                 ap_specialised=Default_specialise})})

and wrap_id_pos_list loc id_pos_list get_field lam =
  let fv = free_variables lam in
  (*Format.eprintf "%a@." Printlambda.lambda lam;
  IdentSet.iter (fun id -> Format.eprintf "%a " Ident.print id) fv;
  Format.eprintf "@.";*)
  let (lam,s) =
    List.fold_left (fun (lam,s) (id',pos,c) ->
      if IdentSet.mem id' fv then
        let id'' = Ident.create (Ident.name id') in
        (Llet(Alias, Pgenval, id'',
              apply_coercion loc Alias c (get_field (Ident.name id') pos),lam),
         Ident.add id' (Lvar id'') s)
      else (lam,s))
      (lam, Ident.empty) id_pos_list
  in
  if s == Ident.empty then lam else subst_lambda s lam


(* Compose two coercions
   apply_coercion c1 (apply_coercion c2 e) behaves like
   apply_coercion (compose_coercions c1 c2) e. *)

let rec compose_coercions c1 c2 =
  match (c1, c2) with
    (Tcoerce_none, c2) -> c2
  | (c1, Tcoerce_none) -> c1
  | (Tcoerce_structure (pc1, ids1, runtime_fields1), Tcoerce_structure (pc2, ids2, _runtime_fields2)) ->
      let v2 = Array.of_list pc2 in
      let ids1 =
        List.map (fun (id,pos1,c1) ->
          let (pos2,c2) = v2.(pos1) in (id, pos2, compose_coercions c1 c2))
          ids1
      in
      Tcoerce_structure
        (List.map
          (function (_p1, Tcoerce_primitive _) as x ->
                      x (* (p1, Tcoerce_primitive p) *)
                  | (p1, c1) ->
                      let (p2, c2) = v2.(p1) in (p2, compose_coercions c1 c2))
             pc1,
         ids1 @ ids2,
         runtime_fields1)
  | (Tcoerce_functor(arg1, res1), Tcoerce_functor(arg2, res2)) ->
      Tcoerce_functor(compose_coercions arg2 arg1,
                      compose_coercions res1 res2)
  | (c1, Tcoerce_alias (path, c2)) ->
      Tcoerce_alias (path, compose_coercions c1 c2)
  | (_, _) ->
      fatal_error "Translmod.compose_coercions"

(*
let apply_coercion a b c =
  Format.eprintf "@[<2>apply_coercion@ %a@]@." Includemod.print_coercion b;
  apply_coercion a b c

let compose_coercions c1 c2 =
  let c3 = compose_coercions c1 c2 in
  let open Includemod in
  Format.eprintf "@[<2>compose_coercions@ (%a)@ (%a) =@ %a@]@."
    print_coercion c1 print_coercion c2 print_coercion c3;
  c3
*)

(* Record the primitive declarations occurring in the module compiled *)

let primitive_declarations = ref ([] : Primitive.description list)
let record_primitive = function
  | {val_kind=Val_prim p} ->
      primitive_declarations := p :: !primitive_declarations
  | _ -> ()

(* Utilities for compiling "module rec" definitions *)

let mod_prim name args loc =
  try
    Lapply
      {
      ap_func = transl_normal_path
        (fst (Env.lookup_value (Ldot (Lident "CamlinternalMod", name))
                Env.empty));
      ap_args =  args;
      ap_loc =  loc;
      ap_inlined = Default_inline;
      ap_specialised = Default_specialise;
      }
  with Not_found ->
    fatal_error ("Primitive " ^ name ^ " not found.")

let undefined_location loc =
  let (fname, line, char) = Location.get_pos_info loc.Location.loc_start in
#if true  
  let fname = Filename.basename fname in
#end
  Lconst(Const_block(Lambda.Blk_tuple,
                     [Const_base(Const_string (fname, None));
                      Const_base(Const_int line);
                      Const_base(Const_int char)]))
let cstr_const = 3  
let cstr_non_const = 2
let init_shape modl =
  let add_name x id =
      Const_block (Blk_tuple, [x; Const_base (Const_string (Ident.name id, None))])
  in  
  let module_tag_info : Lambda.tag_info = Blk_constructor {name =  "Module"; num_nonconst = 2; tag = 0} in 
  let value_tag_info : Lambda.tag_info = Blk_constructor { name = "value"; num_nonconst = 2; tag = 1} in 
  let rec init_shape_mod env mty =
    match Mtype.scrape env mty with
      Mty_ident _ ->
        raise Not_found
    | Mty_alias _ ->
        Const_block (value_tag_info, [Const_pointer (0, Pt_module_alias)])
    | Mty_signature sg ->
        Const_block(module_tag_info, [Const_block(Blk_tuple, init_shape_struct env sg)])
    | Mty_functor _ ->
        raise Not_found (* can we do better? *)
  and init_shape_struct env sg =
    match sg with
      [] -> []
    | Sig_value(id, {val_kind=Val_reg; val_type=ty}) :: rem ->
        let init_v =
          match Ctype.expand_head env ty with
            {desc = Tarrow(_,_,_,_)} ->
              Const_pointer (0, Pt_constructor{name = "Function"; const = cstr_const; non_const = cstr_non_const})
          | {desc = Tconstr(p, _, _)} when Path.same p Predef.path_lazy_t ->
              Const_pointer (1, Pt_constructor{name = "Lazy"; const = cstr_const; non_const = cstr_non_const}) 
          | _ -> raise Not_found in
        (add_name init_v id) :: init_shape_struct env rem
    | Sig_value(_, {val_kind=Val_prim _}) :: rem ->
        init_shape_struct env rem
    | Sig_type(id, tdecl, _) :: rem ->
        init_shape_struct (Env.add_type ~check:false id tdecl env) rem
    | Sig_typext _ :: _ ->
        raise Not_found
    | Sig_module(id, md, _) :: rem ->
        (add_name (init_shape_mod env md.md_type) id) ::
        init_shape_struct (Env.add_module_declaration ~check:false
                             id md env) rem
    | Sig_modtype(id, minfo) :: rem ->
        init_shape_struct (Env.add_modtype id minfo env) rem
    | Sig_class (id,_,_) :: rem ->
        (add_name (Const_pointer (2, Pt_constructor{name = "Class";const = cstr_const; non_const = cstr_non_const})) id)
        :: init_shape_struct env rem
    | Sig_class_type _ :: rem ->
        init_shape_struct env rem
  in
  try
    Some(undefined_location modl.mod_loc,
         Lconst(init_shape_mod modl.mod_env modl.mod_type))
  with Not_found ->
    None

(* Reorder bindings to honor dependencies.  *)

type binding_status = Undefined | Inprogress | Defined

let reorder_rec_bindings bindings =
  let id = Array.of_list (List.map (fun (id,_,_,_) -> id) bindings)
  and loc = Array.of_list (List.map (fun (_,loc,_,_) -> loc) bindings)
  and init = Array.of_list (List.map (fun (_,_,init,_) -> init) bindings)
  and rhs = Array.of_list (List.map (fun (_,_,_,rhs) -> rhs) bindings) in
  let fv = Array.map Lambda.free_variables rhs in
  let num_bindings = Array.length id in
  let status = Array.make num_bindings Undefined in
  let res = ref [] in
  let rec emit_binding i =
    match status.(i) with
      Defined -> ()
    | Inprogress -> raise(Error(loc.(i), Circular_dependency id.(i)))
    | Undefined ->
        if init.(i) = None then begin
          status.(i) <- Inprogress;
          for j = 0 to num_bindings - 1 do
            if IdentSet.mem id.(j) fv.(i) then emit_binding j
          done
        end;
        res := (id.(i), init.(i), rhs.(i)) :: !res;
        status.(i) <- Defined in
  for i = 0 to num_bindings - 1 do
    match status.(i) with
      Undefined -> emit_binding i
    | Inprogress -> assert false
    | Defined -> ()
  done;
  List.rev !res

(* Generate lambda-code for a reordered list of bindings *)

let eval_rec_bindings bindings cont =
  let rec bind_inits = function
    [] ->
      bind_strict bindings
  | (_id, None, _rhs) :: rem ->
      bind_inits rem
  | (id, Some(loc, shape), _rhs) :: rem ->
      Llet(Strict, Pgenval, id,
           mod_prim "init_mod" [loc; shape] Location.none,       
           bind_inits rem)
  and bind_strict = function
    [] ->
      patch_forwards bindings
  | (id, None, rhs) :: rem ->
      Llet(Strict, Pgenval, id, rhs, bind_strict rem)
  | (_id, Some _, _rhs) :: rem ->
      bind_strict rem
  and patch_forwards = function
    [] ->
      cont
  | (_id, None, _rhs) :: rem ->
      patch_forwards rem
  | (id, Some(_loc, shape), rhs) :: rem ->
      Lsequence(
            mod_prim "update_mod" [shape; Lvar id; rhs] Location.none,
            patch_forwards rem)
  in
    bind_inits bindings

let eval_rec_bindings = ref eval_rec_bindings
let compile_recmodule compile_rhs bindings cont =
  !eval_rec_bindings
    (reorder_rec_bindings
       (List.map
          (fun {mb_id=id; mb_expr=modl; mb_loc=loc; _} ->
            (id, modl.mod_loc, init_shape modl, compile_rhs id modl loc))
          bindings))
    cont

(* Extract the list of "value" identifiers bound by a signature.
   "Value" identifiers are identifiers for signature components that
   correspond to a run-time value: values, extensions, modules, classes.
   Note: manifest primitives do not correspond to a run-time value! *)

let rec bound_value_identifiers = function
    [] -> []
  | Sig_value(id, {val_kind = Val_reg}) :: rem ->
      id :: bound_value_identifiers rem
  | Sig_typext(id, _, _) :: rem -> id :: bound_value_identifiers rem
  | Sig_module(id, _, _) :: rem -> id :: bound_value_identifiers rem
  | Sig_class(id, _, _) :: rem -> id :: bound_value_identifiers rem
  | _ :: rem -> bound_value_identifiers rem


(* Compile one or more functors, merging curried functors to produce
   multi-argument functors.  Any [@inline] attribute on a functor that is
   merged must be consistent with any other [@inline] attribute(s) on the
   functor(s) being merged with.  Such an attribute will be placed on the
   resulting merged functor. *)

let merge_inline_attributes attr1 attr2 loc =
  match Lambda.merge_inline_attributes attr1 attr2 with
  | Some attr -> attr
  | None -> raise (Error (loc, Conflicting_inline_attributes))

let merge_functors mexp coercion root_path =
  let rec merge mexp coercion path acc inline_attribute =
    let finished = acc, mexp, path, coercion, inline_attribute in
    match mexp.mod_desc with
    | Tmod_functor (param, _, _, body) ->
      let inline_attribute' =
        Translattribute.get_inline_attribute mexp.mod_attributes
      in
      let arg_coercion, res_coercion =
        match coercion with
        | Tcoerce_none -> Tcoerce_none, Tcoerce_none
        | Tcoerce_functor (arg_coercion, res_coercion) ->
          arg_coercion, res_coercion
        | _ -> fatal_error "Translmod.merge_functors: bad coercion"
      in
      let loc = mexp.mod_loc in
      let path = functor_path path param in
      let inline_attribute =
        merge_inline_attributes inline_attribute inline_attribute' loc
      in
      merge body res_coercion path ((param, loc, arg_coercion) :: acc)
        inline_attribute
    | _ -> finished
  in
  merge mexp coercion root_path [] Default_inline

let export_identifiers  : Ident.t list ref = ref []
let get_export_identifiers () = 
  !export_identifiers

let rec compile_functor mexp coercion root_path loc =
  let functor_params_rev, body, body_path, res_coercion, inline_attribute =
    merge_functors mexp coercion root_path
  in
  assert (List.length functor_params_rev >= 1);  (* cf. [transl_module] *)
  let params, body =
    List.fold_left (fun (params, body) (param, loc, arg_coercion) ->
        let param' = Ident.rename param in
        let arg = apply_coercion loc Alias arg_coercion (Lvar param') in
        let params = param' :: params in
        let body = Llet (Alias, Pgenval, param, arg, body) in
        params, body)
      ([], transl_module res_coercion body_path body)
      functor_params_rev
  in
  Lfunction {
    params;
    attr = {
      inline = inline_attribute;
      specialise = Default_specialise;
      is_a_functor = true;
      stub = false;
    };
    loc;
    body;
  }

(* Compile a module expression *)

and transl_module cc rootpath mexp =
  List.iter (Translattribute.check_attribute_on_module mexp)
    mexp.mod_attributes;
  let loc = mexp.mod_loc in
  match mexp.mod_type with
    Mty_alias (Mta_absent, _) -> apply_coercion loc Alias cc lambda_module_alias
  | _ ->
      match mexp.mod_desc with
        Tmod_ident (path,_) ->
          apply_coercion loc Strict cc
            (transl_module_path ~loc mexp.mod_env path)
      | Tmod_structure str ->
          fst (transl_struct loc [] cc rootpath str)
      | Tmod_functor _ ->
            compile_functor mexp cc rootpath loc
      | Tmod_apply(funct, arg, ccarg) ->
          let inlined_attribute, funct =
            Translattribute.get_and_remove_inlined_attribute_on_module funct
          in
            apply_coercion loc Strict cc
            (Lapply{
                    ap_loc=loc;
                    ap_func=transl_module Tcoerce_none None funct;
                    ap_args=[transl_module ccarg None arg];
                    ap_inlined=inlined_attribute;
                    ap_specialised=Default_specialise})
      | Tmod_constraint(arg, _, _, ccarg) ->
          transl_module (compose_coercions cc ccarg) rootpath arg
      | Tmod_unpack(arg, _) ->
          apply_coercion loc Strict cc (Translcore.transl_exp arg)

and transl_struct loc fields cc rootpath str =
  transl_structure loc fields cc rootpath str.str_final_env str.str_items

and transl_structure loc fields cc rootpath final_env = function
    [] ->
      let is_top_root_path = is_top rootpath in 
      let body, size =
        match cc with
          Tcoerce_none ->            
            let block_fields = 
                (List.fold_left (fun acc id  -> begin
                      (if is_top_root_path then 
                         export_identifiers :=  id :: !export_identifiers);
                      (Lvar id :: acc) end) [] fields ) in 
            Lprim(Pmakeblock(
              (if is_top_root_path then Blk_module_export !export_identifiers else 
                Blk_module (List.rev_map (fun id -> id.Ident.name) fields))),
              block_fields, loc),
              List.length fields
        | Tcoerce_structure(pos_cc_list, id_pos_list, runtime_fields) ->
                (* Do not ignore id_pos_list ! *)
            (*Format.eprintf "%a@.@[" Includemod.print_coercion cc;
            List.iter (fun l -> Format.eprintf "%a@ " Ident.print l)
              fields;
            Format.eprintf "@]@.";*)
            assert (List.length runtime_fields = List.length pos_cc_list);
            let v = Ext_array.reverse_of_list fields in
            let get_field pos = Lvar v.(pos)
            and ids = List.fold_right IdentSet.add fields IdentSet.empty in
            let get_field_name _name = get_field in 
            let result = List.fold_right
              (fun  (pos, cc) code ->
                 begin match cc with
                 | Tcoerce_primitive p -> 
                     (if is_top rootpath then 
                        export_identifiers := p.pc_id:: !export_identifiers);
                     (transl_primitive p.pc_loc p.pc_desc p.pc_env p.pc_type None :: code)
                 | _ -> 
                     (if is_top rootpath then 
                        export_identifiers :=  v.(pos) :: !export_identifiers);
                     (apply_coercion loc Strict cc (get_field pos) :: code)
                 end)
              pos_cc_list [] in             
            let lam =
              Lprim(Pmakeblock((if is_top_root_path then Blk_module_export !export_identifiers else Blk_module runtime_fields)), 
                   result, loc)
            and id_pos_list =
              Ext_list.filter id_pos_list (fun (id,_,_) -> not (IdentSet.mem id ids))                
            in
            wrap_id_pos_list loc id_pos_list get_field_name lam,
              List.length pos_cc_list
        | _ ->
            fatal_error "Translmod.transl_structure"
      in
      (* This debugging event provides information regarding the structure
         items. It is ignored by the OCaml debugger but is used by
         Js_of_ocaml to preserve variable names. *)
      body,
      size
  | item :: rem ->
      match item.str_desc with
      | Tstr_eval (expr, _) ->
          let body, size =
            transl_structure loc fields cc rootpath final_env rem
          in
          Lsequence(transl_exp expr, body), size
      | Tstr_value(rec_flag, pat_expr_list) ->
          let ext_fields = rev_let_bound_idents pat_expr_list @ fields in
          let body, size =
            transl_structure loc ext_fields cc rootpath final_env rem
          in
          transl_let rec_flag pat_expr_list body, size
      | Tstr_primitive descr ->
          record_primitive descr.val_val;
          transl_structure loc fields cc rootpath final_env rem
      | Tstr_type _ ->
          transl_structure loc fields cc rootpath final_env rem
      | Tstr_typext(tyext) ->
          let ids = List.map (fun ext -> ext.ext_id) tyext.tyext_constructors in
          let body, size =
            transl_structure loc (List.rev_append ids fields)
              cc rootpath final_env rem
          in
          transl_type_extension item.str_env rootpath tyext body, size
      | Tstr_exception ext ->
          let id = ext.ext_id in
          let path = field_path rootpath id in
          let body, size =
            transl_structure loc (id :: fields) cc rootpath final_env rem
          in
          Llet(Strict, Pgenval, id,
               transl_extension_constructor item.str_env path ext, body),
          size
      | Tstr_module mb ->
          let id = mb.mb_id in
          let body, size =
            transl_structure loc ( if Typemod.rescript_hide mb then fields else id::fields) cc rootpath final_env rem
          in
          let module_body =
            transl_module Tcoerce_none (field_path rootpath id) mb.mb_expr
          in
          let module_body =
            Translattribute.add_inline_attribute module_body mb.mb_loc
                                                 mb.mb_attributes
          in
          Llet(pure_module mb.mb_expr, Pgenval, id,
               module_body,
               body), size
      | Tstr_recmodule bindings ->
          let ext_fields =
            List.rev_append (List.map (fun mb -> mb.mb_id) bindings) fields
          in
          let body, size =
            transl_structure loc ext_fields cc rootpath final_env rem
          in
          let lam =
            compile_recmodule
              (fun id modl _loc ->

                   transl_module Tcoerce_none (field_path rootpath id) modl
              )
              bindings
              body
          in
          lam, size
      | Tstr_class _ -> assert false      
      | Tstr_include incl ->
          let ids = bound_value_identifiers incl.incl_type in
          let modl = incl.incl_mod in
          let mid = Ident.create "include" in
          let rec rebind_idents pos newfields = function
              [] ->
                transl_structure loc newfields cc rootpath final_env rem
            | id :: ids ->
                let body, size =
                  rebind_idents (pos + 1) (id :: newfields) ids
                in
                Llet(Alias, Pgenval, id,
                     Lprim(Pfield (pos, Fld_module {name = Ident.name id}) , [Lvar mid], incl.incl_loc), body),
                size
          in
          let body, size = rebind_idents 0 fields ids in
          Llet(pure_module modl, Pgenval, mid,
               transl_module Tcoerce_none None modl, body),
          size

      | Tstr_modtype _
      | Tstr_open _
      | Tstr_class_type _
      | Tstr_attribute _ ->
          transl_structure loc fields cc rootpath final_env rem

and pure_module m =
  match m.mod_desc with
    Tmod_ident _ -> Alias
  | Tmod_constraint (m,_,_,_) -> pure_module m
  | _ -> Strict

(* Update forward declaration in Translcore *)
let _ =
  Translcore.transl_module := transl_module

(* Introduce dependencies on modules referenced only by "external". *)

let scan_used_globals lam =
  let globals = ref Ident.Set.empty in
  let rec scan lam =
    Lambda.iter scan lam;
    match lam with
      Lprim ((Pgetglobal id | Psetglobal id), _, _) ->
        globals := Ident.Set.add id !globals
    | _ -> ()
  in
  scan lam; !globals

let required_globals ~flambda body =
  let globals = scan_used_globals body in
  let add_global id req =
    if not flambda && Ident.Set.mem id globals then
      req
    else
      Ident.Set.add id req
  in
  let required =
    Hashtbl.fold
      (fun path _ -> add_global (Path.head path)) used_primitives
      (if flambda then globals else Ident.Set.empty)
  in
  let required =
    List.fold_right add_global (Env.get_required_globals ()) required
  in
  Env.reset_required_globals ();
  Hashtbl.clear used_primitives;
  required

(* Compile an implementation *)

let transl_implementation_flambda module_name (str, cc) =
  primitive_declarations := [];
  Hashtbl.clear used_primitives;
  let module_id = Ident.create_persistent module_name in
  let body, size =
    transl_struct Location.none [] cc
                   (global_path module_id) str
  in
  { module_ident = module_id;
    main_module_block_size = size;
    required_globals = required_globals ~flambda:true body;
    code = body }

let transl_implementation module_name (str, cc) =
  let implementation =
    transl_implementation_flambda module_name (str, cc)
  in
  let code =
    Lprim (Psetglobal implementation.module_ident, [implementation.code],
           Location.none)
  in
  { implementation with code }

(* Build the list of value identifiers defined by a toplevel structure
   (excluding primitive declarations). *)

(* second level idents (module M = struct ... let id = ... end),
   and all sub-levels idents *)
(* A variant of transl_structure used to compile toplevel structure definitions
   for the native-code compiler. Store the defined values in the fields
   of the global as soon as they are defined, in order to reduce register
   pressure.  Also rewrites the defining expressions so that they
   refer to earlier fields of the structure through the fields of
   the global, not by their names.
   "map" is a table from defined idents to (pos in global block, coercion).
   "prim" is a list of (pos in global block, primitive declaration). *)

let transl_store_subst = ref Ident.empty
  (** In the native toplevel, this reference is threaded through successive
      calls of transl_store_structure *)



(* Compile an implementation using transl_store_structure
   (for the native-code compiler). *)


(* Compile a toplevel phrase *)

let toploop_ident = Ident.create_persistent "Toploop"
let aliased_idents = ref Ident.empty


(* Error report *)

open Format

let report_error ppf = function
    Circular_dependency id ->
      fprintf ppf
        "@[Cannot safely evaluate the definition@ \
         of the recursively-defined module %a@]"
        Printtyp.ident id
  | Conflicting_inline_attributes ->
      fprintf ppf
        "@[Conflicting ``inline'' attributes@]"

let () =
  Location.register_error_of_exn
    (function
      | Error (loc, err) ->
        Some (Location.error_of_printer loc report_error err)
      | _ ->
        None
    )

let reset () =
  export_identifiers := [];
  primitive_declarations := [];
  transl_store_subst := Ident.empty;
  toploop_ident.Ident.flags <- 0;
  aliased_idents := Ident.empty;
  Env.reset_required_globals ();
  Hashtbl.clear used_primitives
