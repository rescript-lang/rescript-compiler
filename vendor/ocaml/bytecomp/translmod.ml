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
open Translobj
open Translcore
open Translclass

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
  | Tcoerce_structure(pos_cc_list, id_pos_list) ->
      name_lambda strict arg (fun id ->
        let get_field pos = Lprim(Pfield (pos, Fld_na),[Lvar id], loc) in
        let lam =
          Lprim(Pmakeblock(0, Lambda.default_tag_info, Immutable, None),
                List.map (apply_coercion_field loc get_field) pos_cc_list,
                loc)
        in
        wrap_id_pos_list loc id_pos_list get_field lam)
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
      Lfunction{kind = Curried; params = List.rev params;
                attr = { default_function_attribute with
                         is_a_functor = true;
                         stub = true; };
                loc = loc;
                body = apply_coercion
                         loc Strict cc_res
                         (Lapply{ap_should_be_tailcall=false;
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
              apply_coercion loc Alias c (get_field pos),lam),
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
  | (Tcoerce_structure (pc1, ids1), Tcoerce_structure (pc2, ids2)) ->
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
         ids1 @ ids2)
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
let bs_init_mod args loc : Lambda.lambda =  
  Lprim(Pccall (Primitive.simple 
    ~name:"#init_mod"
    ~arity:2
    ~alloc:true), args, loc)
let bs_update_mod args loc : Lambda.lambda =
  Lprim(Pccall (Primitive.simple
    ~name:"#update_mod"
    ~arity:3
    ~alloc:true), args, loc)

let mod_prim name args loc =
  if !Clflags.bs_only then
    if name = "init_mod" then
      bs_init_mod args loc
    else if name = "update_mod" then
      bs_update_mod args loc
    else assert false
  else
  try
    Lapply
      {
      ap_func = transl_normal_path
        (fst (Env.lookup_value (Ldot (Lident "CamlinternalMod", name))
                Env.empty));
      ap_args =  args;
      ap_loc =  loc;
      ap_should_be_tailcall = false;
      ap_inlined = Default_inline;
      ap_specialised = Default_specialise;
      }
  with Not_found ->
    fatal_error ("Primitive " ^ name ^ " not found.")

let undefined_location loc =
  let (fname, line, char) = Location.get_pos_info loc.Location.loc_start in
#if true then  
  let fname = Filename.basename fname in
#end
  Lconst(Const_block(0, Lambda.default_tag_info,
                     [Const_base(Const_string (fname, None));
                      Const_base(Const_int line);
                      Const_base(Const_int char)]))

let init_shape modl =
  let rec init_shape_mod env mty =
    match Mtype.scrape env mty with
      Mty_ident _ ->
        raise Not_found
    | Mty_alias _ ->
        Const_block (1, Lambda.default_tag_info, [Const_pointer (0, Lambda.Pt_module_alias)])
    | Mty_signature sg ->
        Const_block(0, Lambda.default_tag_info, [Const_block(0, Lambda.default_tag_info, init_shape_struct env sg)])
    | Mty_functor _ ->
        raise Not_found (* can we do better? *)
  and init_shape_struct env sg =
    match sg with
      [] -> []
    | Sig_value(_id, {val_kind=Val_reg; val_type=ty}) :: rem ->
        let init_v =
          match Ctype.expand_head env ty with
            {desc = Tarrow(_,_,_,_)} ->
              Const_pointer (0, Lambda.default_pointer_info) (* camlinternalMod.Function *)
          | {desc = Tconstr(p, _, _)} when Path.same p Predef.path_lazy_t ->
              Const_pointer (1, Lambda.default_pointer_info) (* camlinternalMod.Lazy *)
          | _ -> raise Not_found in
        init_v :: init_shape_struct env rem
    | Sig_value(_, {val_kind=Val_prim _}) :: rem ->
        init_shape_struct env rem
    | Sig_value _ :: _rem ->
        assert false
    | Sig_type(id, tdecl, _) :: rem ->
        init_shape_struct (Env.add_type ~check:false id tdecl env) rem
    | Sig_typext _ :: _ ->
        raise Not_found
    | Sig_module(id, md, _) :: rem ->
        init_shape_mod env md.md_type ::
        init_shape_struct (Env.add_module_declaration ~check:false
                             id md env) rem
    | Sig_modtype(id, minfo) :: rem ->
        init_shape_struct (Env.add_modtype id minfo env) rem
    | Sig_class _ :: rem ->
        Const_pointer (2, Lambda.default_pointer_info) (* camlinternalMod.Class *)
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

let compile_recmodule compile_rhs bindings cont =
  eval_rec_bindings
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


(* Code to translate class entries in a structure *)

let transl_class_bindings cl_list =
  let ids = List.map (fun (ci, _) -> ci.ci_id_class) cl_list in
  (ids,
   List.map
     (fun ({ci_id_class=id; ci_expr=cl; ci_virt=vf}, meths) ->
       (id, transl_class ids id meths cl vf))
     cl_list)

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
    kind = Curried;
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
    Mty_alias (Mta_absent, _) -> apply_coercion loc Alias cc lambda_unit
  | _ ->
      match mexp.mod_desc with
        Tmod_ident (path,_) ->
          apply_coercion loc Strict cc
            (transl_module_path ~loc mexp.mod_env path)
      | Tmod_structure str ->
          fst (transl_struct loc [] cc rootpath str)
      | Tmod_functor _ ->
          oo_wrap mexp.mod_env true (fun () ->
            compile_functor mexp cc rootpath loc) ()
      | Tmod_apply(funct, arg, ccarg) ->
          let inlined_attribute, funct =
            Translattribute.get_and_remove_inlined_attribute_on_module funct
          in
          oo_wrap mexp.mod_env true
            (apply_coercion loc Strict cc)
            (Lapply{ap_should_be_tailcall=false;
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
      let body, size =
        match cc with
          Tcoerce_none ->
            let fields = List.rev fields in 
            let field_names = List.map (fun id -> id.Ident.name) fields in 
            Lprim(Pmakeblock(0, Lambda.Blk_module (Some field_names), Immutable, None),
                (List.fold_right (fun id acc -> begin
                      (if is_top rootpath then 
                         export_identifiers :=  id :: !export_identifiers);
                      (Lvar id :: acc) end) fields [])  , loc),
              List.length fields
        | Tcoerce_structure(pos_cc_list, id_pos_list) ->
                (* Do not ignore id_pos_list ! *)
            (*Format.eprintf "%a@.@[" Includemod.print_coercion cc;
            List.iter (fun l -> Format.eprintf "%a@ " Ident.print l)
              fields;
            Format.eprintf "@]@.";*)
            let v = Array.of_list (List.rev fields) in
            let get_field pos = Lvar v.(pos)
            and ids = List.fold_right IdentSet.add fields IdentSet.empty in
            let (result, names) = List.fold_right
              (fun  (pos, cc) (code, name) ->
                 begin match cc with
                 | Tcoerce_primitive p -> 
                     (if is_top rootpath then 
                        export_identifiers := p.pc_id:: !export_identifiers);
                     (transl_primitive p.pc_loc p.pc_desc p.pc_env p.pc_type None :: code, p.pc_desc.prim_name ::name)
                 | _ -> 
                     (if is_top rootpath then 
                        export_identifiers :=  v.(pos) :: !export_identifiers);
                     (apply_coercion loc Strict cc (get_field pos) :: code, v.(pos).Ident.name :: name)
                 end)
              pos_cc_list ([], [])in             
            let lam =
              Lprim(Pmakeblock(0, Blk_module (Some names), Immutable, None), 
                   result, loc)
            and id_pos_list =
              List.filter (fun (id,_,_) -> not (IdentSet.mem id ids))
                id_pos_list
            in
            wrap_id_pos_list loc id_pos_list get_field lam,
              List.length pos_cc_list
        | _ ->
            fatal_error "Translmod.transl_structure"
      in
      (* This debugging event provides information regarding the structure
         items. It is ignored by the OCaml debugger but is used by
         Js_of_ocaml to preserve variable names. *)
      (if !Clflags.debug && not !Clflags.bs_only then
         Levent(body,
                {lev_loc = loc;
                 lev_kind = Lev_pseudo;
                 lev_repr = None;
                 lev_env = Env.summary final_env})
       else
         body),
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
            transl_structure loc (id :: fields) cc rootpath final_env rem
          in
          let module_body =
            transl_module Tcoerce_none (field_path rootpath id) mb.mb_expr
          in
          let module_body =
            Translattribute.add_inline_attribute module_body mb.mb_loc
                                                 mb.mb_attributes
          in
          let module_body =
#if true then            
            if !Clflags.bs_only then module_body
            else
#end            
            Levent (module_body, {
              lev_loc = mb.mb_loc;
              lev_kind = Lev_module_definition id;
              lev_repr = None;
              lev_env = Env.summary Env.empty;
            })
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
              (fun id modl loc ->
                 let module_body =
                   transl_module Tcoerce_none (field_path rootpath id) modl
                 in
#if true then            
                 if !Clflags.bs_only then module_body
                 else
#end                    
                 Levent (module_body, {
                   lev_loc = loc;
                   lev_kind = Lev_module_definition id;
                   lev_repr = None;
                   lev_env = Env.summary Env.empty;
                 }))
              bindings
              body
          in
          lam, size
      | Tstr_class cl_list ->
          let (ids, class_bindings) = transl_class_bindings cl_list in
          let body, size =
            transl_structure loc (List.rev_append ids fields)
              cc rootpath final_env rem
          in
          Lletrec(class_bindings, body), size
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
                     Lprim(Pfield (pos, Fld_na), [Lvar mid], incl.incl_loc), body),
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
  reset_labels ();
  primitive_declarations := [];
  Hashtbl.clear used_primitives;
  let module_id = Ident.create_persistent module_name in
  let body, size =
    Translobj.transl_label_init
      (fun () -> transl_struct Location.none [] cc
                   (global_path module_id) str)
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

let rec defined_idents = function
    [] -> []
  | item :: rem ->
    match item.str_desc with
    | Tstr_eval _ -> defined_idents rem
    | Tstr_value(_rec_flag, pat_expr_list) ->
      let_bound_idents pat_expr_list @ defined_idents rem
    | Tstr_primitive _ -> defined_idents rem
    | Tstr_type _ -> defined_idents rem
    | Tstr_typext tyext ->
      List.map (fun ext -> ext.ext_id) tyext.tyext_constructors
      @ defined_idents rem
    | Tstr_exception ext -> ext.ext_id :: defined_idents rem
    | Tstr_module mb -> mb.mb_id :: defined_idents rem
    | Tstr_recmodule decls ->
      List.map (fun mb -> mb.mb_id) decls @ defined_idents rem
    | Tstr_modtype _ -> defined_idents rem
    | Tstr_open _ -> defined_idents rem
    | Tstr_class cl_list ->
      List.map (fun (ci, _) -> ci.ci_id_class) cl_list @ defined_idents rem
    | Tstr_class_type _ -> defined_idents rem
    | Tstr_include incl ->
      bound_value_identifiers incl.incl_type @ defined_idents rem
    | Tstr_attribute _ -> defined_idents rem

(* second level idents (module M = struct ... let id = ... end),
   and all sub-levels idents *)
let rec more_idents = function
    [] -> []
  | item :: rem ->
    match item.str_desc with
    | Tstr_eval _ -> more_idents rem
    | Tstr_value _ -> more_idents rem
    | Tstr_primitive _ -> more_idents rem
    | Tstr_type _ -> more_idents rem
    | Tstr_typext _ -> more_idents rem
    | Tstr_exception _ -> more_idents rem
    | Tstr_recmodule _ -> more_idents rem
    | Tstr_modtype _ -> more_idents rem
    | Tstr_open _ -> more_idents rem
    | Tstr_class _ -> more_idents rem
    | Tstr_class_type _ -> more_idents rem
    | Tstr_include{incl_mod={mod_desc =
                             Tmod_constraint ({mod_desc = Tmod_structure str},
                                              _, _, _)}} ->
        all_idents str.str_items @ more_idents rem
    | Tstr_include _ -> more_idents rem
    | Tstr_module {mb_expr={mod_desc = Tmod_structure str}}
    | Tstr_module{mb_expr={mod_desc =
                             Tmod_constraint ({mod_desc = Tmod_structure str},
                                              _, _, _)}} ->
        all_idents str.str_items @ more_idents rem
    | Tstr_module _ -> more_idents rem
    | Tstr_attribute _ -> more_idents rem

and all_idents = function
    [] -> []
  | item :: rem ->
    match item.str_desc with
    | Tstr_eval _ -> all_idents rem
    | Tstr_value(_rec_flag, pat_expr_list) ->
      let_bound_idents pat_expr_list @ all_idents rem
    | Tstr_primitive _ -> all_idents rem
    | Tstr_type _ -> all_idents rem
    | Tstr_typext tyext ->
      List.map (fun ext -> ext.ext_id) tyext.tyext_constructors
      @ all_idents rem
    | Tstr_exception ext -> ext.ext_id :: all_idents rem
    | Tstr_recmodule decls ->
      List.map (fun mb -> mb.mb_id) decls @ all_idents rem
    | Tstr_modtype _ -> all_idents rem
    | Tstr_open _ -> all_idents rem
    | Tstr_class cl_list ->
      List.map (fun (ci, _) -> ci.ci_id_class) cl_list @ all_idents rem
    | Tstr_class_type _ -> all_idents rem

    | Tstr_include{incl_type; incl_mod={mod_desc =
                             Tmod_constraint ({mod_desc = Tmod_structure str},
                                              _, _, _)}} ->
        bound_value_identifiers incl_type @ all_idents str.str_items @ all_idents rem
    | Tstr_include incl ->
      bound_value_identifiers incl.incl_type @ all_idents rem

    | Tstr_module {mb_id;mb_expr={mod_desc = Tmod_structure str}}
    | Tstr_module{mb_id;
                  mb_expr={mod_desc =
                             Tmod_constraint ({mod_desc = Tmod_structure str},
                                              _, _, _)}} ->
        mb_id :: all_idents str.str_items @ all_idents rem
    | Tstr_module mb -> mb.mb_id :: all_idents rem
    | Tstr_attribute _ -> all_idents rem


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

let nat_toplevel_name id =
  try match Ident.find_same id !transl_store_subst with
    | Lprim(Pfield (pos,_), [Lprim(Pgetglobal glob, [], _)], _) -> (glob,pos)
    | _ -> raise Not_found
  with Not_found ->
    fatal_error("Translmod.nat_toplevel_name: " ^ Ident.unique_name id)

let field_of_str loc str =
  let ids = Array.of_list (defined_idents str.str_items) in
  fun (pos, cc) ->
    match cc with
    | Tcoerce_primitive { pc_loc; pc_desc; pc_env; pc_type; } ->
        transl_primitive pc_loc pc_desc pc_env pc_type None
    | _ -> apply_coercion loc Strict cc (Lvar ids.(pos))


let transl_store_structure glob map prims str =
  let rec transl_store rootpath subst = function
    [] ->
      transl_store_subst := subst;
        lambda_unit
    | item :: rem ->
        match item.str_desc with
        | Tstr_eval (expr, _attrs) ->
            Lsequence(subst_lambda subst (transl_exp expr),
                      transl_store rootpath subst rem)
        | Tstr_value(rec_flag, pat_expr_list) ->
            let ids = let_bound_idents pat_expr_list in
            let lam =
              transl_let rec_flag pat_expr_list (store_idents Location.none ids)
            in
            Lsequence(subst_lambda subst lam,
                      transl_store rootpath (add_idents false ids subst) rem)
        | Tstr_primitive descr ->
            record_primitive descr.val_val;
            transl_store rootpath subst rem
        | Tstr_type _ ->
            transl_store rootpath subst rem
        | Tstr_typext(tyext) ->
            let ids =
              List.map (fun ext -> ext.ext_id) tyext.tyext_constructors
            in
            let lam =
              transl_type_extension item.str_env rootpath tyext
                                    (store_idents Location.none ids)
            in
            Lsequence(subst_lambda subst lam,
                      transl_store rootpath (add_idents false ids subst) rem)
        | Tstr_exception ext ->
            let id = ext.ext_id in
            let path = field_path rootpath id in
            let lam = transl_extension_constructor item.str_env path ext in
            Lsequence(Llet(Strict, Pgenval, id, subst_lambda subst lam,
                           store_ident ext.ext_loc id),
                      transl_store rootpath (add_ident false id subst) rem)
        | Tstr_module{mb_id=id;mb_loc=loc;
                      mb_expr={mod_desc = Tmod_structure str} as mexp;
                      mb_attributes} ->
            List.iter (Translattribute.check_attribute_on_module mexp)
              mb_attributes;
            let lam =
              transl_store (field_path rootpath id) subst str.str_items
            in
            (* Careful: see next case *)
            let subst = !transl_store_subst in
            Lsequence(lam,
                      Llet(Strict, Pgenval, id,
                           subst_lambda subst
                             (Lprim(Pmakeblock(0, Lambda.default_tag_info, Immutable, None),
                                    List.map (fun id -> Lvar id)
                                      (defined_idents str.str_items), loc)),
                           Lsequence(store_ident loc id,
                                     transl_store rootpath
                                                  (add_ident true id subst)
                                                  rem)))
        | Tstr_module{
            mb_id=id;mb_loc=loc;
            mb_expr= {
              mod_desc = Tmod_constraint (
                  {mod_desc = Tmod_structure str} as mexp, _, _,
                  (Tcoerce_structure (map, _) as _cc))};
            mb_attributes
          } ->
            (*    Format.printf "coerc id %s: %a@." (Ident.unique_name id)
                                Includemod.print_coercion cc; *)
            List.iter (Translattribute.check_attribute_on_module mexp)
              mb_attributes;
            let lam =
              transl_store (field_path rootpath id) subst str.str_items
            in
            (* Careful: see next case *)
            let subst = !transl_store_subst in
            let field = field_of_str loc str in
            Lsequence(lam,
                      Llet(Strict, Pgenval, id,
                           subst_lambda subst
                             (Lprim(Pmakeblock(0, Lambda.default_tag_info, Immutable, None),
                                    List.map field map, loc)),
                           Lsequence(store_ident loc id,
                                     transl_store rootpath
                                                  (add_ident true id subst)
                                                  rem)))
        | Tstr_module{mb_id=id; mb_expr=modl; mb_loc=loc; mb_attributes} ->
            let lam =
              Translattribute.add_inline_attribute
                (transl_module Tcoerce_none (field_path rootpath id) modl)
                loc mb_attributes
            in
            (* Careful: the module value stored in the global may be different
               from the local module value, in case a coercion is applied.
               If so, keep using the local module value (id) in the remainder of
               the compilation unit (add_ident true returns subst unchanged).
               If not, we can use the value from the global
               (add_ident true adds id -> Pgetglobal... to subst). *)
            Llet(Strict, Pgenval, id, subst_lambda subst lam,
                 Lsequence(store_ident loc id,
                           transl_store rootpath (add_ident true id subst) rem))
        | Tstr_recmodule bindings ->
            let ids = List.map (fun mb -> mb.mb_id) bindings in
            compile_recmodule
              (fun id modl _loc ->
                 subst_lambda subst
                   (transl_module Tcoerce_none
                      (field_path rootpath id) modl))
              bindings
              (Lsequence(store_idents Location.none ids,
                         transl_store rootpath (add_idents true ids subst) rem))
        | Tstr_class cl_list ->
            let (ids, class_bindings) = transl_class_bindings cl_list in
            let lam =
              Lletrec(class_bindings, store_idents Location.none ids)
            in
            Lsequence(subst_lambda subst lam,
                      transl_store rootpath (add_idents false ids subst) rem)

        | Tstr_include{
            incl_loc=loc;
            incl_mod= {
              mod_desc = Tmod_constraint (
                  ({mod_desc = Tmod_structure str} as mexp), _, _,
                  (Tcoerce_structure (map, _)))};
            incl_attributes;
            incl_type;
          } ->
            List.iter (Translattribute.check_attribute_on_module mexp)
              incl_attributes;
            (* Shouldn't we use mod_attributes instead of incl_attributes?
               Same question for the Tstr_module cases above, btw. *)
            let lam =
              transl_store None subst str.str_items
                (* It is tempting to pass rootpath instead of None
                   in order to give a more precise name to exceptions
                   in the included structured, but this would introduce
                   a difference of behavior compared to bytecode. *)
            in
            let subst = !transl_store_subst in
            let field = field_of_str loc str in
            let ids0 = bound_value_identifiers incl_type in
            let rec loop ids args =
              match ids, args with
              | [], [] ->
                  transl_store rootpath (add_idents true ids0 subst) rem
              | id :: ids, arg :: args ->
                  Llet(Alias, Pgenval, id, subst_lambda subst (field arg),
                       Lsequence(store_ident loc id,
                                 loop ids args))
              | _ -> assert false
            in
            Lsequence(lam, loop ids0 map)


        | Tstr_include incl ->
            let ids = bound_value_identifiers incl.incl_type in
            let modl = incl.incl_mod in
            let mid = Ident.create "include" in
            let loc = incl.incl_loc in
            let rec store_idents pos = function
                [] -> transl_store rootpath (add_idents true ids subst) rem
              | id :: idl ->
                  Llet(Alias, Pgenval, id, Lprim(Pfield (pos, Fld_na), [Lvar mid], loc),
                       Lsequence(store_ident loc id,
                                 store_idents (pos + 1) idl))
            in
            Llet(Strict, Pgenval, mid,
                 subst_lambda subst (transl_module Tcoerce_none None modl),
                 store_idents 0 ids)
        | Tstr_modtype _
        | Tstr_open _
        | Tstr_class_type _
        | Tstr_attribute _ ->
            transl_store rootpath subst rem

  and store_ident loc id =
    try
      let (pos, cc) = Ident.find_same id map in
      let init_val = apply_coercion loc Alias cc (Lvar id) in
      Lprim(Psetfield(pos, Pointer, Root_initialization, Fld_set_na),
            [Lprim(Pgetglobal glob, [], loc); init_val],
            loc)
    with Not_found ->
      fatal_error("Translmod.store_ident: " ^ Ident.unique_name id)

  and store_idents loc idlist =
    make_sequence (store_ident loc) idlist

  and add_ident may_coerce id subst =
    try
      let (pos, cc) = Ident.find_same id map in
      match cc with
        Tcoerce_none ->
          Ident.add id
            (Lprim(Pfield (pos, Fld_na),
                   [Lprim(Pgetglobal glob, [], Location.none)],
                   Location.none))
            subst
      | _ ->
          if may_coerce then subst else assert false
    with Not_found ->
      assert false

  and add_idents may_coerce idlist subst =
    List.fold_right (add_ident may_coerce) idlist subst

  and store_primitive (pos, prim) cont =
    Lsequence(Lprim(Psetfield(pos, Pointer, Root_initialization, Fld_set_na),
                    [Lprim(Pgetglobal glob, [], Location.none);
                     transl_primitive Location.none
                       prim.pc_desc prim.pc_env prim.pc_type None],
                    Location.none),
              cont)

  in List.fold_right store_primitive prims
                     (transl_store (global_path glob) !transl_store_subst str)

(* Transform a coercion and the list of value identifiers defined by
   a toplevel structure into a table [id -> (pos, coercion)],
   with [pos] being the position in the global block where the value of
   [id] must be stored, and [coercion] the coercion to be applied to it.
   A given identifier may appear several times
   in the coercion (if it occurs several times in the signature); remember
   to assign it the position of its last occurrence.
   Identifiers that are not exported are assigned positions at the
   end of the block (beyond the positions of all exported idents).
   Also compute the total size of the global block,
   and the list of all primitives exported as values. *)

let build_ident_map restr idlist more_ids =
  let rec natural_map pos map prims = function
      [] ->
        (map, prims, pos)
    | id :: rem ->
        natural_map (pos+1) (Ident.add id (pos, Tcoerce_none) map) prims rem in
  let (map, prims, pos) =
    match restr with
        Tcoerce_none ->
          natural_map 0 Ident.empty [] idlist
      | Tcoerce_structure (pos_cc_list, _id_pos_list) ->
              (* ignore _id_pos_list as the ids are already bound *)
        let idarray = Array.of_list idlist in
        let rec export_map pos map prims undef = function
        [] ->
          natural_map pos map prims undef
          | (_source_pos, Tcoerce_primitive p) :: rem ->
            export_map (pos + 1) map ((pos, p) :: prims) undef rem
          | (source_pos, cc) :: rem ->
            let id = idarray.(source_pos) in
            export_map (pos + 1) (Ident.add id (pos, cc) map)
              prims (list_remove id undef) rem
        in export_map 0 Ident.empty [] idlist pos_cc_list
      | _ ->
        fatal_error "Translmod.build_ident_map"
  in
  natural_map pos map prims more_ids

(* Compile an implementation using transl_store_structure
   (for the native-code compiler). *)

let transl_store_gen module_name ({ str_items = str }, restr) topl =
  reset_labels ();
  primitive_declarations := [];
  Hashtbl.clear used_primitives;
  let module_id = Ident.create_persistent module_name in
  let (map, prims, size) =
    build_ident_map restr (defined_idents str) (more_idents str) in
  let f = function
    | [ { str_desc = Tstr_eval (expr, _attrs) } ] when topl ->
        assert (size = 0);
        subst_lambda !transl_store_subst (transl_exp expr)
    | str -> transl_store_structure module_id map prims str in
  transl_store_label_init module_id size f str
  (*size, transl_label_init (transl_store_structure module_id map prims str)*)

let transl_store_phrases module_name str =
  transl_store_gen module_name (str,Tcoerce_none) true

let transl_store_implementation module_name (str, restr) =
  let s = !transl_store_subst in
  transl_store_subst := Ident.empty;
  let (i, code) = transl_store_gen module_name (str, restr) false in
  transl_store_subst := s;
  { Lambda.main_module_block_size = i;
    code;
    (* module_ident is not used by closure, but this allow to share
       the type with the flambda version *)
    module_ident = Ident.create_persistent module_name;
    required_globals = required_globals ~flambda:true code }

(* Compile a toplevel phrase *)

let toploop_ident = Ident.create_persistent "Toploop"
let toploop_getvalue_pos = 0 (* position of getvalue in module Toploop *)
let toploop_setvalue_pos = 1 (* position of setvalue in module Toploop *)

let aliased_idents = ref Ident.empty

let set_toplevel_unique_name id =
  aliased_idents :=
    Ident.add id (Ident.unique_toplevel_name id) !aliased_idents

let toplevel_name id =
  try Ident.find_same id !aliased_idents
  with Not_found -> Ident.name id

let toploop_getvalue id =
  Lapply{ap_should_be_tailcall=false;
         ap_loc=Location.none;
         ap_func=Lprim(Pfield (toploop_getvalue_pos, Fld_na),
                       [Lprim(Pgetglobal toploop_ident, [], Location.none)],
                       Location.none);
         ap_args=[Lconst(Const_base(Const_string (toplevel_name id, None)))];
         ap_inlined=Default_inline;
         ap_specialised=Default_specialise}

let toploop_setvalue id lam =
  Lapply{ap_should_be_tailcall=false;
         ap_loc=Location.none;
         ap_func=Lprim(Pfield (toploop_setvalue_pos, Fld_na),
                       [Lprim(Pgetglobal toploop_ident, [], Location.none)],
                       Location.none);
         ap_args=[Lconst(Const_base(Const_string (toplevel_name id, None)));
                  lam];
         ap_inlined=Default_inline;
         ap_specialised=Default_specialise}

let toploop_setvalue_id id = toploop_setvalue id (Lvar id)

let close_toplevel_term (lam, ()) =
  IdentSet.fold (fun id l -> Llet(Strict, Pgenval, id,
                                  toploop_getvalue id, l))
                (free_variables lam) lam

let transl_toplevel_item item =
  match item.str_desc with
    Tstr_eval (expr, _)
  | Tstr_value(Nonrecursive,
               [{vb_pat = {pat_desc=Tpat_any};vb_expr = expr}]) ->
      (* special compilation for toplevel "let _ = expr", so
         that Toploop can display the result of the expression.
         Otherwise, the normal compilation would result
         in a Lsequence returning unit. *)
      transl_exp expr
  | Tstr_value(rec_flag, pat_expr_list) ->
      let idents = let_bound_idents pat_expr_list in
      transl_let rec_flag pat_expr_list
        (make_sequence toploop_setvalue_id idents)
  | Tstr_typext(tyext) ->
      let idents =
        List.map (fun ext -> ext.ext_id) tyext.tyext_constructors
      in
      (* we need to use unique name in case of multiple
         definitions of the same extension constructor in the toplevel *)
      List.iter set_toplevel_unique_name idents;
        transl_type_extension item.str_env None tyext
          (make_sequence toploop_setvalue_id idents)
  | Tstr_exception ext ->
      set_toplevel_unique_name ext.ext_id;
      toploop_setvalue ext.ext_id
        (transl_extension_constructor item.str_env None ext)
  | Tstr_module {mb_id=id; mb_expr=modl} ->
      (* we need to use the unique name for the module because of issues
         with "open" (PR#1672) *)
      set_toplevel_unique_name id;
      let lam = transl_module Tcoerce_none (Some(Pident id)) modl in
      toploop_setvalue id lam
  | Tstr_recmodule bindings ->
      let idents = List.map (fun mb -> mb.mb_id) bindings in
      compile_recmodule
        (fun id modl _loc -> transl_module Tcoerce_none (Some(Pident id)) modl)
        bindings
        (make_sequence toploop_setvalue_id idents)
  | Tstr_class cl_list ->
      (* we need to use unique names for the classes because there might
         be a value named identically *)
      let (ids, class_bindings) = transl_class_bindings cl_list in
      List.iter set_toplevel_unique_name ids;
      Lletrec(class_bindings, make_sequence toploop_setvalue_id ids)
  | Tstr_include incl ->
      let ids = bound_value_identifiers incl.incl_type in
      let modl = incl.incl_mod in
      let mid = Ident.create "include" in
      let rec set_idents pos = function
        [] ->
          lambda_unit
      | id :: ids ->
          Lsequence(toploop_setvalue id
                      (Lprim(Pfield (pos, Fld_na), [Lvar mid], Location.none)),
                    set_idents (pos + 1) ids) in
      Llet(Strict, Pgenval, mid,
           transl_module Tcoerce_none None modl, set_idents 0 ids)
  | Tstr_modtype _
  | Tstr_open _
  | Tstr_primitive _
  | Tstr_type _
  | Tstr_class_type _
  | Tstr_attribute _ ->
      lambda_unit

let transl_toplevel_item_and_close itm =
  close_toplevel_term
    (transl_label_init (fun () -> transl_toplevel_item itm, ()))

let transl_toplevel_definition str =
  reset_labels ();
  Hashtbl.clear used_primitives;
  make_sequence transl_toplevel_item_and_close str.str_items

(* Compile the initialization code for a packed library *)

let get_component = function
    None -> Lconst const_unit
  | Some id -> Lprim(Pgetglobal id, [], Location.none)

let transl_package_flambda component_names coercion =
  let size =
    match coercion with
    | Tcoerce_none -> List.length component_names
    | Tcoerce_structure (l, _) -> List.length l
    | Tcoerce_functor _
    | Tcoerce_primitive _
    | Tcoerce_alias _ -> assert false
  in
  size,
  apply_coercion Location.none Strict coercion
    (Lprim(Pmakeblock(0, Lambda.default_tag_info, Immutable, None), (*NOTE: not relevant in flambda *)
           List.map get_component component_names,
           Location.none))

let transl_package component_names target_name coercion =
  let components =
    Lprim(Pmakeblock(0, Lambda.default_tag_info, Immutable, None),
          List.map get_component component_names, Location.none) in
  Lprim(Psetglobal target_name,
        [apply_coercion Location.none Strict coercion components],
        Location.none)
  (*
  let components =
    match coercion with
      Tcoerce_none ->
        List.map get_component component_names
    | Tcoerce_structure (pos_cc_list, id_pos_list) ->
              (* ignore id_pos_list as the ids are already bound *)
        let g = Array.of_list component_names in
        List.map
          (fun (pos, cc) -> apply_coercion Strict cc (get_component g.(pos)))
          pos_cc_list
    | _ ->
        assert false in
  Lprim(Psetglobal target_name, [Lprim(Pmakeblock(0, Immutable), components)])
   *)

let transl_store_package component_names target_name coercion =
  let rec make_sequence fn pos arg =
    match arg with
      [] -> lambda_unit
    | hd :: tl -> Lsequence(fn pos hd, make_sequence fn (pos + 1) tl) in
  match coercion with
    Tcoerce_none ->
      (List.length component_names,
       make_sequence
         (fun pos id ->
           Lprim(Psetfield(pos, Pointer, Root_initialization, Fld_set_na),
                 [Lprim(Pgetglobal target_name, [], Location.none);
                  get_component id],
                 Location.none))
         0 component_names)
  | Tcoerce_structure (pos_cc_list, _id_pos_list) ->
      let components =
        Lprim(Pmakeblock(0, Lambda.default_tag_info, Immutable, None),
              List.map get_component component_names,
              Location.none)
      in
      let blk = Ident.create "block" in
      (List.length pos_cc_list,
       Llet (Strict, Pgenval, blk,
             apply_coercion Location.none Strict coercion components,
             make_sequence
               (fun pos _id ->
                 Lprim(Psetfield(pos, Pointer, Root_initialization, Fld_set_na),
                       [Lprim(Pgetglobal target_name, [], Location.none);
                        Lprim(Pfield (pos, Fld_na), [Lvar blk], Location.none)],
                       Location.none))
               0 pos_cc_list))
  (*
              (* ignore id_pos_list as the ids are already bound *)
      let id = Array.of_list component_names in
      (List.length pos_cc_list,
       make_sequence
         (fun dst (src, cc) ->
           Lprim(Psetfield(dst, false),
                 [Lprim(Pgetglobal target_name, []);
                  apply_coercion Strict cc (get_component id.(src))]))
         0 pos_cc_list)
  *)
  | _ -> assert false

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
