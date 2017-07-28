(** Creates an mli from an annotated ml file. *)

open Path
open Location
open Longident
open Misc
open Parsetree
open Types
open! Typedtree
open Ast_helper

let mli_attr l = Convenience.find_attr "mli" l

let map_flatten f l =
  List.flatten (List.map f l)

let is_abstract = function
  | PStr [{pstr_desc=Pstr_eval({pexp_desc=Pexp_ident{txt=Lident "abstract"}},_)}] -> true
  | _ -> false

let explicit_type_of_expr = function
  | {pexp_desc=Pexp_constraint({pexp_desc=Pexp_ident{txt=Lident id}}, t)} -> [id, t]
  | _ -> []

let explicit_type = function
  | PStr [{pstr_desc=Pstr_eval({pexp_desc=Pexp_tuple el},_)}] -> map_flatten explicit_type_of_expr el
  | PStr [{pstr_desc=Pstr_eval(e,_)}] -> explicit_type_of_expr e
  | _ -> []

let rec structure l : Parsetree.signature =
  map_flatten (structure_item l.str_final_env) l.str_items

and structure_item final_env x : Parsetree.signature =
  match x.str_desc with
  | Tstr_module {mb_name; mb_expr} ->
    begin match module_expr mb_expr with
    | Some mty -> [Sig.module_ (Md.mk mb_name mty)]
    | None -> []
    end
  | Tstr_type l ->
    begin match map_flatten type_declaration l with
    | [] -> []
    | l -> [Sig.type_ l]
    end
  | Tstr_value (_, l) ->
    map_flatten (value_binding final_env) l
  | _ ->
    []

and module_expr x : Parsetree.module_type option =
  match x.mod_desc with
  | Tmod_structure l ->
    (* No explicit signature: use [@@mli] attributes in the sub-structure to define exported components. *)
    begin match structure l with
    | [] -> None
    | l -> Some (Mty.signature l)
    end
  | Tmod_constraint (_, _, Tmodtype_explicit mty, _) ->
    (* Explicit signature: if non-empty, use it for the mli; if empty, drop the sub-module *)
    begin match Untypeast.untype_module_type mty with
    | {pmty_desc=Pmty_signature []} -> None
    | pmty -> Some pmty
    end
  | _ ->
    None

and type_declaration x : Parsetree.type_declaration list =
  match mli_attr x.typ_attributes with
  | None -> []
  | Some attrs ->
    let pdecl = Untypeast.untype_type_declaration x in
    (* If the declaration is marked with [@@mli abstract], make it abstract *)
    let pdecl = if is_abstract attrs then {pdecl with ptype_kind=Ptype_abstract} else pdecl in
    [pdecl]

and value_binding final_env x : Parsetree.signature =
  match mli_attr x.vb_attributes with
  | None -> []
  | Some attrs ->
    match explicit_type attrs with
    | [] ->
      (* No explicit type, use the inferred type for bound identifiers *)
      let ids = let_bound_idents [x] in
      List.map
        (fun id ->
           let ty = typ (Env.find_value (Pident id) final_env).val_type in
           Sig.value (Val.mk (mknoloc (Ident.name id)) ty)
        ) ids
    | l ->
      (* Explicit type given with the syntax [@@mli (x1 : ty1), ..., (xn : tyn)] *)
      List.map (fun (id, ty) -> Sig.value (Val.mk (mknoloc id) ty)) l

and typ x : Parsetree.core_type =
  (* print the inferred type and parse the result again *)
  let t = Printtyp.type_scheme Format.str_formatter x in
  let s = Format.flush_str_formatter t in
  Parse.core_type (Lexing.from_string s)

let mli_of_ml ppf sourcefile =
  Location.input_name := sourcefile;
  Compmisc.init_path false;
  let file = chop_extension_if_any sourcefile in
  let modulename = String.capitalize(Filename.basename file) in
  Env.set_unit_name modulename;
  let inputfile = Pparse.preprocess sourcefile in
  let env = Compmisc.initial_env() in
  let ast = Pparse.file ppf inputfile Parse.implementation Config.ast_impl_magic_number in
  let (str, _coerc) = Typemod.type_implementation sourcefile file modulename env ast in
  let sg = structure str in
  Format.printf "%a@." Pprintast.signature sg

let () =
  mli_of_ml Format.err_formatter Sys.argv.(1)

