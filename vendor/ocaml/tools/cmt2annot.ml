(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                  Fabrice Le Fessant, INRIA Saclay                   *)
(*                                                                     *)
(*  Copyright 2012 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Generate an .annot file from a .cmt file. *)

open Asttypes
open Typedtree

let bind_variables scope =
  object
    inherit Tast_iter.iter as super

    method! pattern pat =
      super # pattern pat;
      match pat.pat_desc with
      | Tpat_var (id, _) | Tpat_alias (_, id, _) ->
          Stypes.record (Stypes.An_ident (pat.pat_loc,
                                          Ident.name id,
                                          Annot.Idef scope))
      | _ -> ()
  end

let bind_bindings scope bindings =
  let o = bind_variables scope in
  List.iter (fun x -> o # pattern x.vb_pat) bindings

let bind_cases l =
  List.iter
    (fun {c_lhs; c_guard; c_rhs} ->
      let loc =
        let open Location in
        match c_guard with
        | None -> c_rhs.exp_loc
        | Some g -> {c_rhs.exp_loc with loc_start=g.exp_loc.loc_start}
      in
      (bind_variables loc) # pattern c_lhs) l

let iterator rebuild_env =
  object(this)
    val scope = Location.none  (* scope of the surrounding structure *)

    inherit Tast_iter.iter as super

    method! class_expr node =
      Stypes.record (Stypes.Ti_class node);
      super # class_expr node

    method! module_expr node =
      Stypes.record (Stypes.Ti_mod node);
      Tast_iter.module_expr {< scope = node.mod_loc >} node

    method! expression exp =
      begin match exp.exp_desc with
      | Texp_ident (path, _, _) ->
          let full_name = Path.name ~paren:Oprint.parenthesized_ident path in
          let env =
            if rebuild_env then
              try
                Env.env_of_only_summary Envaux.env_from_summary exp.exp_env
              with Envaux.Error err ->
                Format.eprintf "%a@." Envaux.report_error err;
                exit 2
            else
              exp.exp_env
          in
          let annot =
            try
              let desc = Env.find_value path env in
              let dloc = desc.Types.val_loc in
              if dloc.Location.loc_ghost then Annot.Iref_external
              else Annot.Iref_internal dloc
            with Not_found ->
              Annot.Iref_external
          in
          Stypes.record
            (Stypes.An_ident (exp.exp_loc, full_name , annot))
      | Texp_let (Recursive, bindings, _) ->
          bind_bindings exp.exp_loc bindings
      | Texp_let (Nonrecursive, bindings, body) ->
          bind_bindings body.exp_loc bindings
      | Texp_match (_, f1, f2, _) ->
        bind_cases f1;
        bind_cases f2
      | Texp_function (_, f, _)
      | Texp_try (_, f) ->
          bind_cases f
      | _ -> ()
      end;
      Stypes.record (Stypes.Ti_expr exp);
      super # expression exp

    method! pattern pat =
      super # pattern pat;
      Stypes.record (Stypes.Ti_pat pat)

    method private structure_item_rem s rem =
      begin match s with
      | {str_desc = Tstr_value (rec_flag, bindings); str_loc = loc} ->
          let open Location in
          let doit loc_start = bind_bindings {scope with loc_start} bindings in
          begin match rec_flag, rem with
          | Recursive, _ -> doit loc.loc_start
          | Nonrecursive, [] -> doit loc.loc_end
          | Nonrecursive,  {str_loc = loc2} :: _ -> doit loc2.loc_start
          end
      | _ ->
          ()
      end;
      Stypes.record_phrase s.str_loc;
      super # structure_item s

    method! structure_item s =
      (* This will be used for Partial_structure_item.
         We don't have here the location of the "next" item,
         this will give a slightly different scope for the non-recursive
         binding case. *)
      this # structure_item_rem s []

    method! structure l =
      let rec loop = function
        | str :: rem -> this # structure_item_rem str rem; loop rem
        | [] -> ()
      in
      loop l.str_items

(* TODO: support binding for Tcl_fun, Tcl_let, etc *)
  end

let binary_part iter x =
  let open Cmt_format in
  match x with
  | Partial_structure x -> iter # structure x
  | Partial_structure_item x -> iter # structure_item x
  | Partial_expression x -> iter # expression x
  | Partial_pattern x -> iter # pattern x
  | Partial_class_expr x -> iter # class_expr x
  | Partial_signature x -> iter # signature x
  | Partial_signature_item x -> iter # signature_item x
  | Partial_module_type x -> iter # module_type x

let gen_annot target_filename filename
              {Cmt_format.cmt_loadpath; cmt_annots; cmt_use_summaries; _} =
  let open Cmt_format in
  Envaux.reset_cache ();
  Config.load_path := cmt_loadpath;
  let target_filename =
    match target_filename with
    | None -> Some (filename ^ ".annot")
    | Some "-" -> None
    | Some _ -> target_filename
  in
  let iterator = iterator cmt_use_summaries in
  match cmt_annots with
  | Implementation typedtree ->
      iterator # structure typedtree;
      Stypes.dump target_filename
  | Interface _ ->
      Printf.eprintf "Cannot generate annotations for interface file\n%!";
      exit 2
  | Partial_implementation parts ->
      Array.iter (binary_part iterator) parts;
      Stypes.dump target_filename
  | _ ->
      Printf.fprintf stderr "File was generated with an error\n%!";
      exit 2



let gen_ml target_filename filename cmt =
  let (printer, ext) =
    match cmt.Cmt_format.cmt_annots with
      | Cmt_format.Implementation typedtree ->
        (fun ppf -> Pprintast.structure ppf
                                        (Untypeast.untype_structure typedtree)),
        ".ml"
      | Cmt_format.Interface typedtree ->
        (fun ppf -> Pprintast.signature ppf
                                        (Untypeast.untype_signature typedtree)),
        ".mli"
      | _ ->
        Printf.fprintf stderr "File was generated with an error\n%!";
        exit 2
  in
  let target_filename = match target_filename with
      None -> Some (filename ^ ext)
    | Some "-" -> None
    | Some _ -> target_filename
  in
  let oc = match target_filename with
      None -> None
    | Some filename -> Some (open_out filename) in
  let ppf = match oc with
      None -> Format.std_formatter
    | Some oc -> Format.formatter_of_out_channel oc in
  printer ppf;
  Format.pp_print_flush ppf ();
  match oc with
      None -> flush stdout
    | Some oc -> close_out oc
