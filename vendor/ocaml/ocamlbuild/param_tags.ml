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

open My_std

(* Original author: Romain Bardou *)

module StringSet = Set.Make(String)

(* tag name -> tag action (string -> unit) *)
let declared_tags = Hashtbl.create 17

let acknowledged_tags = ref []

let only_once f =
  let instances = ref StringSet.empty in
  fun param ->
    if StringSet.mem param !instances then ()
    else begin
      instances := StringSet.add param !instances;
      f param
    end

let declare name action =
  Hashtbl.add declared_tags name (only_once action)

let parse source tag = Lexers.tag_gen source (lexbuf_of_string tag)

let acknowledge source maybe_loc tag =
  acknowledged_tags := (parse source tag, maybe_loc) :: !acknowledged_tags

let really_acknowledge ?(quiet=false) ((name, param), maybe_loc) =
  match param with
    | None ->
        if Hashtbl.mem declared_tags name && not quiet then
          Log.eprintf "%aWarning: tag %S expects a parameter"
            Loc.print_loc_option maybe_loc name
    | Some param ->
        let actions = List.rev (Hashtbl.find_all declared_tags name) in
        if actions = [] && not quiet then
          Log.eprintf "%aWarning: tag %S does not expect a parameter, \
                       but is used with parameter %S"
            Loc.print_loc_option maybe_loc name param;
        List.iter (fun f -> f param) actions

let partial_init ?quiet source tags =
  let parse_noloc tag = (parse source tag, None) in
  Tags.iter (fun tag -> really_acknowledge ?quiet (parse_noloc tag)) tags

let init () =
  List.iter really_acknowledge (My_std.List.ordered_unique !acknowledged_tags)

let make = Printf.sprintf "%s(%s)"
