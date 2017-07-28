(*
  A toy -ppx rewriter which illustrates code generation based on type
  declarations.  Here, we create builder function from record and sum
  type declarations annotated with attribute [@@builder]: one function
  per record type, one function per constructor of a sum type.

  We recognize some special attributes on record fields (or their associated
  type) and on constructor argument types:

  - [@label id]: specify a label for the parameter of the builder function
    (for records, it is set automatically from the label name
    but it can be overridden).

  - [@opt]: the parameter is optional (this assume that the field/argument
    has an option type).

  - [@default expr]: the parameter is optional, with a default value
    (cannot be used with [@opt]).
*)

module Main : sig end = struct
  open Asttypes
  open! Location
  open Parsetree
  open Ast_helper
  open Ast_helper.Convenience

  let fatal loc s =
    Location.print_error Format.err_formatter loc;
    prerr_endline s;
    exit 2

  let param named name loc attrs =
    let default = find_attr_expr "default" attrs in
    let opt = has_attr "opt" attrs in
    let label =
      match find_attr_expr "label" attrs with
      | None -> if named then name else ""
      | Some e ->
          match get_lid e with
          | Some s -> s
          | None -> fatal e.pexp_loc "'label' attribute must be a string literal"
    in
    let label =
      if default <> None || opt then
        if label = "" then fatal loc "Optional arguments must be named" else "?" ^ label
      else label
    in
    if default <> None && opt then fatal loc "Cannot have both 'opt' and 'default' attributes";
    lam ~label ?default (pvar name), (name, evar name)

  let gen_builder tdecl =
    if has_attr "builder" tdecl.ptype_attributes then
      match tdecl.ptype_kind with
      | Ptype_record fields ->
          let field pld =
            param true pld.pld_name.txt pld.pld_loc (pld.pld_attributes @ pld.pld_type.ptyp_attributes)
          in
          let fields = List.map field fields in
          let body = lam (punit()) (record (List.map snd fields)) in
          let f = List.fold_right (fun (f, _) k -> f k) fields body in
          let s = Str.value Nonrecursive [Vb.mk (pvar tdecl.ptype_name.txt) f] in
          [s]
      | Ptype_variant constrs ->
          let constr {pcd_name={txt=name;_}; pcd_args=args; _} =
            let arg i ty = param false (Printf.sprintf "x%i" i) ty.ptyp_loc ty.ptyp_attributes in
            let args = List.mapi arg args in
            let body = lam (punit()) (constr name (List.map (fun (_, (_, e)) -> e) args)) in
            let f = List.fold_right (fun (f, _) k -> f k) args body in
            let s = Str.value Nonrecursive [Vb.mk (pvar (tdecl.ptype_name.txt ^ "_" ^ name)) f] in
            s
          in
          List.map constr constrs
      | _ -> []
    else
      []

  let gen_builder tdecl =
    with_default_loc tdecl.ptype_loc (fun () -> gen_builder tdecl)

  let builder _args =
    let open Ast_mapper in
    let super = default_mapper in
    {super
     with
      structure =
        (fun this l ->
           List.flatten
             (List.map
                (function
                  | {pstr_desc = Pstr_type tdecls; _} as i ->
                      i :: (List.flatten (List.map gen_builder tdecls))
                  | i -> [this.structure_item this i]
                ) l
             )
        )
    }

  let () = Ast_mapper.run_main builder
end
