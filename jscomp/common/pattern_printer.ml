open Types
open Typedtree
open Parsetree

let mkpat desc = Ast_helper.Pat.mk desc

let untype typed =
  let rec loop pat =
    match pat.pat_desc with
    | Tpat_or (pa, pb, _) -> mkpat (Ppat_or (loop pa, loop pb))
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
  let ps = loop typed in
  ps

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

(* group or patterns from left to right *)
let normalize_or_patterns pat =
  let rec loop pat =
    match pat.ppat_desc with
    | Ppat_or (l, r) ->
        let p = mkpat (Ppat_or (loop l, loop r)) in
        let c = p |> flatten_or_patterns |> join_or_patterns in
        c
    | Ppat_any -> pat
    | Ppat_var _ -> pat
    | Ppat_constant _ -> pat
    | Ppat_interval _ -> pat
    | Ppat_alias (p, _) -> p
    | Ppat_open _ -> pat (* Not produced by typedtree *)
    | Ppat_extension _ -> pat
    | Ppat_constraint _ -> pat
    | Ppat_exception _ -> pat
    | Ppat_unpack _ -> pat
    | Ppat_type _ -> pat
    | Ppat_lazy p -> mkpat (Ppat_lazy (loop p))
    | Ppat_array lst -> mkpat (Ppat_array (List.map loop lst))
    | Ppat_tuple ps ->
        let ps = List.map loop ps in
        mkpat (Ppat_tuple ps)
    | Ppat_variant (lbl, maybe_p) ->
        let maybe_p = Option.map loop maybe_p in
        mkpat (Ppat_variant (lbl, maybe_p))
    | Ppat_record (fields, closed_flag) ->
        let fields =
          List.map (fun field -> (fst field, loop (snd field))) fields
        in
        mkpat (Ppat_record (fields, closed_flag))
    | Ppat_construct (lbl, maybe_p) ->
        let maybe_p = Option.map loop maybe_p in
        mkpat (Ppat_construct (lbl, maybe_p))
  in
  loop pat

let print_pattern typed =
  let pat = typed |> untype |> normalize_or_patterns in
  let doc = Res_printer.printPattern pat Res_comments_table.empty in
  Res_doc.toString ~width:80 doc
