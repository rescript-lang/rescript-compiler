(***********************************************************************)
(*                                                                     *)
(*                             ocamlbuild                              *)
(*                                                                     *)
(*  Nicolas Pouillard, Berke Durak, projet Gallium, INRIA Rocquencourt *)
(*                                                                     *)
(*  Copyright 2007 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)


(* Original author: Nicolas Pouillard *)
open My_std
open Log
open Lexers

type t = Lexers.conf

let acknowledge_config source config =
  let ack (tag, loc) = Param_tags.acknowledge source (Some loc) tag in
  List.iter (fun (_, config) -> List.iter ack config.plus_tags) config

let cache = Hashtbl.create 107
let (configs, add_config) =
  let configs = ref [] in
  (fun () -> !configs),
  (fun source config ->
     acknowledge_config source config;
     configs := config :: !configs;
     Hashtbl.clear cache)

let parse_lexbuf ?dir source lexbuf =
  let conf = Lexers.conf_lines dir source lexbuf in
  add_config source conf

let parse_string ?source s =
  let source = match source with
    | Some source -> source
    | None -> Const.Source.configuration
  in
  parse_lexbuf source (lexbuf_of_string s)

let parse_file ?dir file =
  with_input_file file begin fun ic ->
    let lexbuf = Lexing.from_channel ic in
    set_lexbuf_fname file lexbuf;
    parse_lexbuf ?dir Const.Source.file lexbuf
  end

let key_match = Glob.eval

let apply_config s (config : t) init =
  let add (tag, _loc) = Tags.add tag in
  let remove (tag, _loc) = Tags.remove tag in
  List.fold_left begin fun tags (key, v) ->
    if key_match key s then
      List.fold_right add v.plus_tags (List.fold_right remove v.minus_tags tags)
    else tags
  end init config

let apply_configs s = List.fold_right (apply_config s) (configs ()) Tags.empty

let tags_of_filename s =
  try Hashtbl.find cache s
  with Not_found ->
    let res = apply_configs s in
    let () = Hashtbl.replace cache s res in
    res

let global_tags () = tags_of_filename ""
let has_tag tag = Tags.mem tag (global_tags ())

let tag_file file tags =
  if tags <> [] then parse_string (Printf.sprintf "%S: %s" file (String.concat ", " tags));;

let tag_any tags =
  if tags <> [] then parse_string (Printf.sprintf "true: %s" (String.concat ", " tags));;

let check_tags_usage useful_tags =
  let check_tag (tag, loc) =
    if not (Tags.mem tag useful_tags) then

      Log.eprintf "%aWarning: the tag %S is not used in any flag or dependency \
                   declaration, so it will have no effect; it may be a typo. \
                   Otherwise you can use `mark_tag_used` in your myocamlbuild.ml \
                   to disable this warning."
        Loc.print_loc loc tag
  in
  let check_conf (_, values) =
    List.iter check_tag values.plus_tags;
    List.iter check_tag values.minus_tags;
  in
  List.iter (List.iter check_conf) (configs ())
