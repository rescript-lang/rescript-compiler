(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Maxence Guesdon, projet Cristal, INRIA Rocquencourt        *)
(*                                                                        *)
(*   Copyright 2001 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Environment for finding complete names from relative names. *)

let print_DEBUG s = print_string s ; print_newline ();;

module Name = Odoc_name

(** relative name * complete name *)
type env_element = Name.t * Name.t

type env = {
    env_values : env_element list ;
    env_types : env_element list ;
    env_class_types : env_element list ;
    env_classes : env_element list ;
    env_modules : env_element list ;
    env_module_types : env_element list ;
    env_extensions : env_element list ;
  }

let empty = {
  env_values = [] ;
  env_types = [] ;
  env_class_types = [] ;
  env_classes = [] ;
  env_modules = [] ;
  env_module_types = [] ;
  env_extensions = [] ;
  }

(** Add a signature to an environment.  *)
let rec add_signature env root ?rel signat =
  let qualify id = Name.concat root (Name.from_ident id) in
  let rel_name id =
    let n = Name.from_ident id in
    match rel with
      None -> n
    | Some r -> Name.concat r n
  in
  let f env item =
    match item with
      Types.Sig_value (ident, _) -> { env with env_values = (rel_name ident, qualify ident) :: env.env_values }
    | Types.Sig_type (ident,_,_) -> { env with env_types = (rel_name ident, qualify ident) :: env.env_types }
    | Types.Sig_typext (ident, _, _) -> { env with env_extensions = (rel_name ident, qualify ident) :: env.env_extensions }
    | Types.Sig_module (ident, md, _) ->
        let env2 =
          match md.Types.md_type with (* FIXME: we don't have signature for identifiers *)
            Types.Mty_signature s -> add_signature env (qualify ident) ~rel: (rel_name ident) s
          |  _ -> env
        in
        { env2 with env_modules = (rel_name ident, qualify ident) :: env2.env_modules }
    | Types.Sig_modtype (ident, modtype_decl) ->
        let env2 =
          match modtype_decl.Types.mtd_type with
            None ->
              env
          | Some modtype ->
              match modtype with
                 (* FIXME: we don't have signature for identifiers *)
                Types.Mty_signature s -> add_signature env (qualify ident) ~rel: (rel_name ident) s
              |  _ -> env
        in
        { env2 with env_module_types = (rel_name ident, qualify ident) :: env2.env_module_types }
    | Types.Sig_class (ident, _, _) -> { env with env_classes = (rel_name ident, qualify ident) :: env.env_classes }
    | Types.Sig_class_type (ident, _, _) -> { env with env_class_types = (rel_name ident, qualify ident) :: env.env_class_types }
  in
  List.fold_left f env signat

let add_extension env full_name =
  let simple_name = Name.simple full_name in
  { env with env_extensions = (simple_name, full_name) :: env.env_extensions }

let add_type env full_name =
  let simple_name = Name.simple full_name in
  { env with env_types = (simple_name, full_name) :: env.env_types }

let add_value env full_name =
  let simple_name = Name.simple full_name in
  { env with env_values = (simple_name, full_name) :: env.env_values }

let add_module env full_name =
  let simple_name = Name.simple full_name in
  { env with env_modules = (simple_name, full_name) :: env.env_modules }

let add_module_type env full_name =
  let simple_name = Name.simple full_name in
  { env with env_module_types = (simple_name, full_name) :: env.env_module_types }

let add_class env full_name =
  let simple_name = Name.simple full_name in
  { env with
    env_classes = (simple_name, full_name) :: env.env_classes ;
    (* we also add a type 'cause the class name may appear as a type *)
    env_types = (simple_name, full_name) :: env.env_types
  }

let add_class_type env full_name =
  let simple_name = Name.simple full_name in
  { env with
    env_class_types = (simple_name, full_name) :: env.env_class_types ;
    (* we also add a type 'cause the class type name may appear as a type *)
    env_types = (simple_name, full_name) :: env.env_types
  }

let full_module_name env n =
  try List.assoc n env.env_modules
  with Not_found ->
    print_DEBUG ("Module "^n^" not found with env=");
    List.iter (fun (sn, fn) -> print_DEBUG ("("^sn^", "^fn^")")) env.env_modules;
    n

let full_module_type_name env n =
  try List.assoc n env.env_module_types
  with Not_found ->
    print_DEBUG ("Module "^n^" not found with env=");
    List.iter (fun (sn, fn) -> print_DEBUG ("("^sn^", "^fn^")")) env.env_modules;
    n

let full_module_or_module_type_name env n =
  try List.assoc n env.env_modules
  with Not_found -> full_module_type_name env n

let full_type_name env n =
  try
    let full = List.assoc n env.env_types in
(*    print_string ("type "^n^" is "^full);
    print_newline ();*)
    full
  with Not_found ->
(*    print_string ("type "^n^" not found");
    print_newline ();*)
    n

let full_value_name env n =
  try List.assoc n env.env_values
  with Not_found -> n

let full_extension_constructor_name env n =
  try List.assoc n env.env_extensions
  with Not_found ->
    print_DEBUG ("Extension "^n^" not found with env=");
    List.iter (fun (sn, fn) -> print_DEBUG ("("^sn^", "^fn^")")) env.env_extensions;
    n

let full_class_name env n =
  try List.assoc n env.env_classes
  with Not_found ->
    print_DEBUG ("Class "^n^" not found with env=");
    List.iter (fun (sn, fn) -> print_DEBUG ("("^sn^", "^fn^")")) env.env_classes;
    n

let full_class_type_name env n =
  try List.assoc n env.env_class_types
  with Not_found ->
    print_DEBUG ("Class type "^n^" not found with env=");
    List.iter (fun (sn, fn) -> print_DEBUG ("("^sn^", "^fn^")")) env.env_class_types;
    n

let full_class_or_class_type_name env n =
  try List.assoc n env.env_classes
  with Not_found -> full_class_type_name env n

let subst_type env t =
(*
  print_string "Odoc_env.subst_type\n";
  print_env_types env ;
  print_newline ();
*)
  Printtyp.mark_loops t;
  let deja_vu = ref [] in
  let rec iter t =
    if List.memq t !deja_vu then () else begin
      deja_vu := t :: !deja_vu;
      Btype.iter_type_expr iter t;
      match t.Types.desc with
      | Types.Tconstr (p, [_], _) when Path.same p Predef.path_option ->
          ()
      | Types.Tconstr (p, l, a) ->
          let new_p =
            Odoc_name.to_path (full_type_name env (Odoc_name.from_path p)) in
          t.Types.desc <- Types.Tconstr (new_p, l, a)
      | Types.Tpackage (p, n, l) ->
          let new_p =
            Odoc_name.to_path (full_module_type_name env (Odoc_name.from_path p)) in
          t.Types.desc <- Types.Tpackage (new_p, n, l)
      | Types.Tobject (_, ({contents=Some(p,tyl)} as r)) ->
          let new_p =
            Odoc_name.to_path (full_type_name env (Odoc_name.from_path p)) in
          r := Some (new_p, tyl)
      | Types.Tvariant ({Types.row_name=Some(p, tyl)} as row) ->
          let new_p =
            Odoc_name.to_path (full_type_name env (Odoc_name.from_path p)) in
          t.Types.desc <-
            Types.Tvariant {row with Types.row_name=Some(new_p, tyl)}
      | _ ->
          ()
    end
  in
  iter t;
  t


let subst_module_type env t =
  let rec iter t =
    match t with
      Types.Mty_ident p ->
        let new_p = Odoc_name.to_path (full_module_type_name env (Odoc_name.from_path p)) in
        Types.Mty_ident new_p
    | Types.Mty_alias _
    | Types.Mty_signature _ ->
        t
    | Types.Mty_functor (id, mt1, mt2) ->
        Types.Mty_functor (id, Misc.may_map iter mt1, iter mt2)
  in
  iter t

let subst_class_type env t =
  let rec iter t =
    match t with
      Types.Cty_constr (p,texp_list,ct) ->
        let new_p = Odoc_name.to_path (full_type_name env (Odoc_name.from_path p)) in
        let new_texp_list = List.map (subst_type env) texp_list in
        let new_ct = iter ct in
        Types.Cty_constr (new_p, new_texp_list, new_ct)
    | Types.Cty_signature _ ->
        (* we don't handle vals and methods *)
        t
    | Types.Cty_arrow (l, texp, ct) ->
        let new_texp = subst_type env texp in
        let new_ct = iter ct in
        Types.Cty_arrow (l, new_texp, new_ct)
  in
  iter t
