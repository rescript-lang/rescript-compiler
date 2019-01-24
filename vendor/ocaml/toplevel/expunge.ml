(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* "Expunge" a toplevel by removing compiler modules from the global map.
   Usage: expunge <source file> <dest file> <names of modules to keep> *)

open Misc

module StringSet =
  Set.Make(struct
    type t = string
    let compare = compare
  end)

let is_exn =
  let h = Hashtbl.create 64 in
  Array.iter (fun n -> Hashtbl.add h n ()) Runtimedef.builtin_exceptions;
  Hashtbl.mem h

let to_keep = ref StringSet.empty

let negate = Sys.argv.(3) = "-v"

let keep =
  if negate then fun name -> is_exn name || not (StringSet.mem name !to_keep)
  else fun name -> is_exn name || (StringSet.mem name !to_keep)

let expunge_map tbl =
  Symtable.filter_global_map (fun id -> keep (Ident.name id)) tbl

let expunge_crcs tbl =
  List.filter (fun (unit, _crc) -> keep unit) tbl

let main () =
  let input_name = Sys.argv.(1) in
  let output_name = Sys.argv.(2) in
  for i = (if negate then 4 else 3) to Array.length Sys.argv - 1 do
    to_keep := StringSet.add (String.capitalize_ascii Sys.argv.(i)) !to_keep
  done;
  let ic = open_in_bin input_name in
  Bytesections.read_toc ic;
  let toc = Bytesections.toc() in
  let pos_first_section = Bytesections.pos_first_section ic in
  let oc =
    open_out_gen [Open_wronly; Open_creat; Open_trunc; Open_binary] 0o777
                 output_name in
  (* Copy the file up to the symbol section as is *)
  seek_in ic 0;
  copy_file_chunk ic oc pos_first_section;
  (* Copy each section, modifying the symbol section in passing *)
  Bytesections.init_record oc;
  List.iter
    (fun (name, len) ->
      begin match name with
        "SYMB" ->
          let global_map = (input_value ic : Symtable.global_map) in
          output_value oc (expunge_map global_map)
      | "CRCS" ->
          let crcs = (input_value ic : (string * Digest.t option) list) in
          output_value oc (expunge_crcs crcs)
      | _ ->
          copy_file_chunk ic oc len
      end;
      Bytesections.record oc name)
    toc;
  (* Rewrite the toc and trailer *)
  Bytesections.write_toc_and_trailer oc;
  (* Done *)
  close_in ic;
  close_out oc

let _ = Printexc.catch main (); exit 0
