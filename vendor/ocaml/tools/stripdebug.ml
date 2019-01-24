(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Gallium, INRIA Paris                  *)
(*                                                                        *)
(*   Copyright 2015 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Copy a bytecode executable, removing debugging information
   and #! header from the copy.
   Usage: stripdebug <source file> <dest file>
*)

open Printf
open Misc

let stripdebug infile outfile =
  let ic = open_in_bin infile in
  Bytesections.read_toc ic;
  let toc = Bytesections.toc() in
  let pos_first_section = Bytesections.pos_first_section ic in
  let oc =
    open_out_gen [Open_wronly; Open_creat; Open_trunc; Open_binary] 0o777
                 outfile in
  (* Skip the #! header, going straight to the first section. *)
  seek_in ic pos_first_section;
  (* Copy each section except DBUG *)
  Bytesections.init_record oc;
  List.iter
    (fun (name, len) ->
      if name = "DBUG" then begin
        seek_in ic (in_channel_length ic + len)
      end else begin
        copy_file_chunk ic oc len;
        Bytesections.record oc name
      end)
    toc;
  (* Rewrite the toc and trailer *)
  Bytesections.write_toc_and_trailer oc;
  (* Done *)
  close_in ic;
  close_out oc

let _ =
  if Array.length Sys.argv = 3
  then stripdebug Sys.argv.(1) Sys.argv.(2)
  else begin
    eprintf "Usage: stripdebug <source file> <destination file>\n";
    exit 2
  end
