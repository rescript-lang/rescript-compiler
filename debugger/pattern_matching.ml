(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*          Jerome Vouillon, projet Cristal, INRIA Rocquencourt        *)
(*          OCaml port by John Malecki and Xavier Leroy                *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(************************ Simple pattern matching **********************)

open Debugger_config
(*open Primitives*)
open Misc
(*open Const*)
(*open Globals*)
(*open Builtins*)
open Typedtree
(*open Modules*)
(*open Symtable*)
(*open Value*)
open Parser_aux

(*
let rec find_constr tag = function
    [] ->
      fatal_error "find_constr: unknown constructor for this type"
  | constr::rest ->
      match constr.info.cs_tag with
        ConstrRegular(t, _) ->
          if t == tag then constr else find_constr tag rest
      | ConstrExtensible _ ->
          fatal_error "find_constr: extensible"

let find_exception tag =
  let (qualid, stamp) = get_exn_of_num tag in
  let rec select_exn = function
    [] ->
      raise Not_found
  | constr :: rest ->
      match constr.info.cs_tag with
        ConstrExtensible(_,st) ->
          if st == stamp then constr else select_exn rest
      | ConstrRegular(_,_) ->
          fatal_error "find_exception: regular" in
  select_exn(hashtbl__find_all (find_module qualid.qual).mod_constrs qualid.id)
*)

let error_matching () =
  prerr_endline "Pattern matching failed";
  raise Toplevel

(*
let same_name {qualid = name1} =
  function
    GRname name2 ->
      (name2 = "") || (name1.id = name2)
  | GRmodname name2 ->
      name1 = name2

let check_same_constr constr constr2 =
  try
    if not (same_name constr constr2) then
      error_matching ()
  with
    Desc_not_found ->
      prerr_endline "Undefined constructor.";
      raise Toplevel
*)

let rec pattern_matching pattern obj ty =
  match pattern with
    P_dummy ->
      []
  | P_variable var ->
      [var, obj, ty]
  | _ ->
    match (Ctype.repr ty).desc with
      Tvar | Tarrow _ ->
        error_matching ()
    | Ttuple(ty_list) ->
        (match pattern with
           P_tuple pattern_list ->
             pattern_matching_list pattern_list obj ty_list
         | P_nth (n, patt) ->
             if n >= List.length ty_list then
               (prerr_endline "Out of range."; raise Toplevel);
             pattern_matching patt (Debugcom.get_field obj n)
                              (List.nth ty_list n)
         | _ ->
             error_matching ())
    | Tconstr(cstr, [ty_arg],_) when same_type_constr cstr constr_type_list ->
        (match pattern with
           P_list pattern_list ->
             let (last, list) =
               it_list
                 (fun (current, list) pattern ->
                    if value_tag current = 0 then error_matching ();
                    (Debugcom.get_field current 1,
                     (pattern, Debugcom.get_field current 0)::list))
                 (obj, [])
                 pattern_list
             in
               if value_tag last <> 0 then error_matching ();
               flat_map
                 (function (x, y) -> pattern_matching x y ty_arg)
                 (rev list)
         | P_nth (n, patt) ->
             let rec find k current =
               if value_tag current = 0 then
                 (prerr_endline "Out of range."; raise Toplevel);
               if k = 0 then
                 pattern_matching patt (Debugcom.get_field current 0) ty_arg
               else
                 find (k - 1) (Debugcom.get_field current 1)
             in
               find n obj
         | P_concat (pattern1, pattern2) ->
             if value_tag obj == 0 then error_matching ();
             (pattern_matching pattern1 (Debugcom.get_field obj 0) ty_arg)
                @ (pattern_matching pattern2 (Debugcom.get_field obj 1) ty)
         | _ ->
             error_matching ())
    | Tconstr(cstr, [ty_arg]) when same_type_constr cstr constr_type_vect ->
        (match pattern with
           P_nth (n, patt) ->
             if n >= value_size obj then
               (prerr_endline "Out of range."; raise Toplevel);
             pattern_matching patt (Debugcom.get_field obj n) ty_arg
         | _ ->
             error_matching ())
    | Tconstr(cstr, ty_list) ->
        (match cstr.info.ty_abbr with
           Tabbrev(params, body) ->
             pattern_matching pattern obj (expand_abbrev params body ty_list)
         | _ ->
             match_concrete_type pattern obj cstr ty ty_list)

and match_concrete_type pattern obj cstr ty ty_list =
  let typ_descr =
    type_descr_of_type_constr cstr in
  match typ_descr.info.ty_desc with
    Abstract_type ->
      error_matching ()
  | Variant_type constr_list ->
      let tag = value_tag obj in
        (try
           let constr =
             if same_type_constr cstr constr_type_exn then
               find_exception tag
             else
               find_constr tag constr_list
           in
             let (ty_res, ty_arg) =
               type_pair_instance (constr.info.cs_res, constr.info.cs_arg)
             in
               filter (ty_res, ty);
               match constr.info.cs_kind with
                 Constr_constant ->
                   error_matching ()
               | Constr_regular ->
                   (match pattern with
                      P_constr (constr2, patt) ->
                        check_same_constr constr constr2;
                        pattern_matching patt (Debugcom.get_field obj 0) ty_arg
                    | _ ->
                        error_matching ())
               | Constr_superfluous n ->
                   (match pattern with
                      P_constr (constr2, patt) ->
                        check_same_constr constr constr2;
                        (match patt with
                           P_tuple pattern_list ->
                             pattern_matching_list
                               pattern_list
                               obj
                               (filter_product n ty_arg)
                         | P_nth (n2, patt) ->
                             let ty_list = filter_product n ty_arg in
                               if n2 >= n then
                                 (prerr_endline "Out of range.";
                                  raise Toplevel);
                               pattern_matching
                                 patt
                                 (Debugcom.get_field obj n2)
                                 (List.nth ty_list n2)
                         | P_variable var ->
                             [var,
                              obj,
                              {typ_desc = Tproduct (filter_product n ty_arg);
                               typ_level = generic}]
                         | P_dummy ->
                             []
                         | _ ->
                             error_matching ())
                    | _ ->
                        error_matching ())
         with
           Not_found ->
             error_matching ()
         | Unify ->
             fatal_error "pattern_matching: types should match")
  | Record_type label_list ->
      let match_field (label, patt) =
        let lbl =
          try
            primitives__find
              (function l -> same_name l label)
              label_list
          with Not_found ->
              prerr_endline "Label not found.";
              raise Toplevel
        in
          let (ty_res, ty_arg) =
            type_pair_instance (lbl.info.lbl_res, lbl.info.lbl_arg)
          in
            (try
               filter (ty_res, ty)
             with Unify ->
               fatal_error "pattern_matching: types should match");
            pattern_matching patt (Debugcom.get_field obj lbl.info.lbl_pos)
                             ty_arg
      in
        (match pattern with
           P_record pattern_label_list ->
             flat_map match_field pattern_label_list
         | _ ->
             error_matching ())
  | Abbrev_type(_,_) ->
      fatal_error "pattern_matching: abbrev type"

and pattern_matching_list pattern_list obj ty_list =
  let val_list =
    try
      pair__combine (pattern_list, ty_list)
    with
      Invalid_argument _ -> error_matching ()
  in
    flat_map
      (function (x, y, z) -> pattern_matching x y z)
      (rev
         (snd
            (it_list
               (fun (num, list) (pattern, typ) ->
                  (num + 1, (pattern, Debugcom.get_field obj num, typ)::list))
               (0, [])
               val_list)))
