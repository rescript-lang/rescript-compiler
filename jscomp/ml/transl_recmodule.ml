open Types
open Typedtree
open Lambda

type error = Circular_dependency of Ident.t

exception Error of Location.t * error
(* Reorder bindings to honor dependencies.  *)

(* Utilities for compiling "module rec" definitions *)

let mod_prim name args loc =
  try
    Lapply
      {
        ap_func =
          transl_normal_path
            (fst
               (Env.lookup_value
                  (Ldot (Lident "CamlinternalMod", name))
                  Env.empty));
        ap_args = args;
        ap_loc = loc;
        ap_inlined = Default_inline;
      }
  with Not_found -> Misc.fatal_error ("Primitive " ^ name ^ " not found.")

let undefined_location loc =
  let fname, line, char = Location.get_pos_info loc.Location.loc_start in
  let fname = Filename.basename fname in
  Lconst
    (Const_block
       ( Lambda.Blk_tuple,
         [
           Const_base (Const_string (fname, None));
           Const_base (Const_int line);
           Const_base (Const_int char);
         ] ))

let cstr_const = 3

let cstr_non_const = 2

let init_shape modl =
  let add_name x id =
    Const_block
      (Blk_tuple, [ x; Const_base (Const_string (Ident.name id, None)) ])
  in
  let module_tag_info : Lambda.tag_info =
    Blk_constructor { name = "Module"; num_nonconst = 2; tag = 0 }
  in
  let value_tag_info : Lambda.tag_info =
    Blk_constructor { name = "value"; num_nonconst = 2; tag = 1 }
  in
  let rec init_shape_mod env mty =
    match Mtype.scrape env mty with
    | Mty_ident _ -> raise Not_found
    | Mty_alias _ ->
        Const_block (value_tag_info, [ Const_pointer (0, Pt_module_alias) ])
    | Mty_signature sg ->
        Const_block
          ( module_tag_info,
            [ Const_block (Blk_tuple, init_shape_struct env sg) ] )
    | Mty_functor _ -> raise Not_found
  (* can we do better? *)
  and init_shape_struct env sg =
    match sg with
    | [] -> []
    | Sig_value (id, { val_kind = Val_reg; val_type = ty }) :: rem ->
        let init_v =
          match Ctype.expand_head env ty with
          | { desc = Tarrow (_, _, _, _) } ->
              Const_pointer
                ( 0,
                  Pt_constructor
                    {
                      name = "Function";
                      const = cstr_const;
                      non_const = cstr_non_const;
                    } )
          | { desc = Tconstr (p, _, _) } when Path.same p Predef.path_lazy_t ->
              Const_pointer
                ( 1,
                  Pt_constructor
                    {
                      name = "Lazy";
                      const = cstr_const;
                      non_const = cstr_non_const;
                    } )
          | _ -> raise Not_found
        in
        add_name init_v id :: init_shape_struct env rem
    | Sig_value (_, { val_kind = Val_prim _ }) :: rem ->
        init_shape_struct env rem
    | Sig_type (id, tdecl, _) :: rem ->
        init_shape_struct (Env.add_type ~check:false id tdecl env) rem
    | Sig_typext _ :: _ -> raise Not_found
    | Sig_module (id, md, _) :: rem ->
        add_name (init_shape_mod env md.md_type) id
        ::
        init_shape_struct
          (Env.add_module_declaration ~check:false id md env)
          rem
    | Sig_modtype (id, minfo) :: rem ->
        init_shape_struct (Env.add_modtype id minfo env) rem
    | Sig_class _ :: _ ->
        assert false
    | Sig_class_type _ :: rem -> init_shape_struct env rem
  in
  try
    Some
      ( undefined_location modl.mod_loc,
        Lconst (init_shape_mod modl.mod_env modl.mod_type) )
  with Not_found -> None

type binding_status = Undefined | Inprogress | Defined

let reorder_rec_bindings bindings =
  let id = Array.of_list (List.map (fun (id, _, _, _) -> id) bindings)
  and loc = Array.of_list (List.map (fun (_, loc, _, _) -> loc) bindings)
  and init = Array.of_list (List.map (fun (_, _, init, _) -> init) bindings)
  and rhs = Array.of_list (List.map (fun (_, _, _, rhs) -> rhs) bindings) in
  let fv = Array.map Lambda.free_variables rhs in
  let num_bindings = Array.length id in
  let status = Array.make num_bindings Undefined in
  let res = ref [] in
  let rec emit_binding i =
    match status.(i) with
    | Defined -> ()
    | Inprogress -> raise (Error (loc.(i), Circular_dependency id.(i)))
    | Undefined ->
        if init.(i) = None then (
          status.(i) <- Inprogress;
          for j = 0 to num_bindings - 1 do
            if IdentSet.mem id.(j) fv.(i) then emit_binding j
          done);
        res := (id.(i), init.(i), rhs.(i)) :: !res;
        status.(i) <- Defined
  in
  for i = 0 to num_bindings - 1 do
    match status.(i) with
    | Undefined -> emit_binding i
    | Inprogress -> assert false
    | Defined -> ()
  done;
  List.rev !res

let eval_rec_bindings bindings cont =
  let rec bind_inits = function
    | [] -> bind_strict bindings
    | (_id, None, _rhs) :: rem -> bind_inits rem
    | (id, Some (loc, shape), _rhs) :: rem ->
        Llet
          ( Strict,
            Pgenval,
            id,
            mod_prim "init_mod" [ loc; shape ] Location.none,
            bind_inits rem )
  and bind_strict = function
    | [] -> patch_forwards bindings
    | (id, None, rhs) :: rem -> Llet (Strict, Pgenval, id, rhs, bind_strict rem)
    | (_id, Some _, _rhs) :: rem -> bind_strict rem
  and patch_forwards = function
    | [] -> cont
    | (_id, None, _rhs) :: rem -> patch_forwards rem
    | (id, Some (_loc, shape), rhs) :: rem ->
        Lsequence
          ( mod_prim "update_mod" [ shape; Lvar id; rhs ] Location.none,
            patch_forwards rem )
  in
  bind_inits bindings

let eval_rec_bindings = ref eval_rec_bindings

let compile_recmodule compile_rhs bindings cont =
  !eval_rec_bindings
    (reorder_rec_bindings
       (List.map
          (fun { mb_id = id; mb_expr = modl; mb_loc = loc; _ } ->
            (id, modl.mod_loc, init_shape modl, compile_rhs id modl loc))
          bindings))
    cont

open Format

let report_error ppf = function
  | Circular_dependency id ->
      fprintf ppf
        "@[Cannot safely evaluate the definition@ of the recursively-defined \
         module %a@]"
        Printtyp.ident id

let () =
  Location.register_error_of_exn (function
    | Error (loc, err) -> Some (Location.error_of_printer loc report_error err)
    | _ -> None)
