let untagged = "unboxed"

let has_untagged (attrs: Parsetree.attributes) =
  Ext_list.exists attrs (function ({txt}, _) -> txt = untagged)

let process_untagged (attrs : Parsetree.attributes) =
  let st = ref false in
  Ext_list.iter attrs (fun ({txt}, _) ->
      match txt with
      | "unboxed" -> st := true
      | _ -> ());
  !st

type error = InvalidVariantAsAnnotation | Duplicated_bs_as
exception Error of Location.t * error

let process_literal (attrs : Parsetree.attributes) =
  let st : Lambda.literal option ref = ref None in
  Ext_list.iter attrs (fun (({txt; loc}, payload)) ->
      match txt with
      | "bs.as" | "as" ->
        if !st = None then (
          (match Ast_payload.is_single_string payload with
          | None -> ()
          | Some (s, _dec) ->
            st := Some (String s));
          (match Ast_payload.is_single_int payload with
          | None -> ()
          | Some i ->
            st := Some (Int i));
          (match Ast_payload.is_single_float payload with
          | None -> ()
          | Some f ->
            st := Some (Float f));
          (match Ast_payload.is_single_bool payload with
          | None -> ()
          | Some b ->
            st := Some (Bool b));
          (match Ast_payload.is_single_ident payload with
          | None -> ()
          | Some (Lident "null") ->
            st := Some Null
          | Some (Lident "undefined") ->
            st := Some Undefined
          | Some _ -> raise (Error (loc, InvalidVariantAsAnnotation)));
          if !st = None then raise (Error (loc, InvalidVariantAsAnnotation)))
        else raise (Error (loc, Duplicated_bs_as))
      | _ -> ());
  !st


let report_error ppf =
  let open Format in
  function
  | InvalidVariantAsAnnotation ->
    fprintf ppf "A variant case annotation @as(...) must be a string or integer, \
     boolean, null, undefined"
| Duplicated_bs_as ->
    fprintf ppf "duplicate @as "


let () =
  Location.register_error_of_exn
    (function
      | Error (loc, err) ->
        Some (Location.error_of_printer loc report_error err)
      | _ ->
        None
    )

let check_well_formed (_cstrs: Parsetree.constructor_declaration list) =
  ()
