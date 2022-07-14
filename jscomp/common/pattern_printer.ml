open Types
open Typedtree
open Parsetree

module Non_empty_list : sig
  type 'a t = 'a * 'a list

  val concat : 'a t -> 'a t -> 'a t
end = struct
  type 'a t = 'a * 'a list

  let concat l r =
    let lhead, ltail = l in
    let rhead, rtail = r in
    (lhead, ltail @ (rhead :: rtail))
end

let mkpat desc = Ast_helper.Pat.mk desc

let join_or_patterns = function
  | p, [] -> p
  | init_l, init_r :: t ->
      let initial_value = mkpat (Ppat_or (init_l, init_r)) in
      let result =
        List.fold_left (fun l r -> mkpat (Ppat_or (l, r))) initial_value t
      in
      result

let flatten_or_patterns p =
  let rec loop p =
    match p.ppat_desc with
    | Ppat_or (l, r) ->
        let lhs_patterns = loop l in
        let rhs_patterns = loop r in
        Non_empty_list.concat lhs_patterns rhs_patterns
    | _ -> (p, [])
  in
  loop p

let untype typed =
  let rec loop pat =
    match pat.pat_desc with
    | Tpat_or (pa, pb, _) ->
        mkpat (Ppat_or (loop pa, loop pb))
        |> flatten_or_patterns |> join_or_patterns
    | Tpat_any | Tpat_var _ -> mkpat Ppat_any
    | Tpat_constant c -> mkpat (Ppat_constant (Untypeast.constant c))
    | Tpat_alias (p, _, _) -> loop p
    | Tpat_tuple lst -> mkpat (Ppat_tuple (List.map loop lst))
    | Tpat_construct (cstr_lid, cstr, lst) ->
        let lid = { cstr_lid with txt = Longident.Lident cstr.cstr_name } in
        let arg =
          match List.map loop lst with
          | [] -> None
          | [ p ] -> Some p
          | lst -> Some (mkpat (Ppat_tuple lst))
        in
        mkpat (Ppat_construct (lid, arg))
    | Tpat_variant (label, p_opt, _row_desc) ->
        let arg = Option.map loop p_opt in
        mkpat (Ppat_variant (label, arg))
    | Tpat_record (subpatterns, closed_flag) ->
        let fields =
          List.map
            (fun (_, lbl, p) ->
              (mknoloc (Longident.Lident lbl.lbl_name), loop p))
            subpatterns
        in
        mkpat (Ppat_record (fields, closed_flag))
    | Tpat_array lst -> mkpat (Ppat_array (List.map loop lst))
    | Tpat_lazy p -> mkpat (Ppat_lazy (loop p))
  in
  loop typed

let print_pattern typed =
  let pat = untype typed in
  let doc = Res_printer.printPattern pat Res_comments_table.empty in
  Res_doc.toString ~width:80 doc
