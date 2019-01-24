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

(* Compare two bytecode executables for equality.
   Ignore loader prefix and debug infos. *)

open Printf

let readtoc ic =
  Bytesections.read_toc ic;
  (Bytesections.toc(), Bytesections.pos_first_section ic)

type cmpresult = Same | Differ of int

let rec cmpbytes ic1 ic2 len ofs =
  if len <= 0 then Same else begin
    let c1 = input_char ic1 and c2 = input_char ic2 in
    if c1 = c2 then cmpbytes ic1 ic2 (len - 1) (ofs + 1) else Differ ofs
  end

let skip_section name =
  name = "DBUG"

let cmpbyt file1 file2 =
  let ic1 = open_in_bin file1 in
  let (toc1, pos1) = readtoc ic1 in
  let ic2 = open_in_bin file2 in
  let (toc2, pos2) = readtoc ic2 in
  seek_in ic1 pos1;
  seek_in ic2 pos2;
  let rec cmpsections t1 t2 =
    match t1, t2 with
    | [], [] ->
        true
    | (name1, len1) :: t1, t2  when skip_section name1 ->
        seek_in ic1 (pos_in ic1 + len1);
         cmpsections t1 t2
    | t1, (name2, len2) :: t2  when skip_section name2 ->
        seek_in ic2 (pos_in ic2 + len2);
        cmpsections t1 t2
    | [], _  ->
        eprintf "%s has more sections than %s\n" file2 file1;
        false
    | _,  [] ->
        eprintf "%s has more sections than %s\n" file1 file2;
        false
    | (name1, len1) :: t1, (name2, len2) :: t2 ->
        if name1 <> name2 then begin
          eprintf "Section mismatch: %s (in %s) / %s (in %s)\n"
                  name1 file1 name2 file2;
          false
        end else if len1 <> len2 then begin
          eprintf "Length of section %s differ: %d (in %s) / %d (in %s)\n"
                  name1 len1 file1 len2 file2;
          false
        end else begin
          match cmpbytes ic1 ic2 len1 0 with
          | Differ ofs ->
              eprintf "Files %s and %s differ: section %s, offset %d\n"
                      file1 file2 name1 ofs;
              false
          | Same ->
              cmpsections t1 t2
        end
  in
  let res = cmpsections toc1 toc2 in
  close_in ic1; close_in ic2;
  res

let _ =
  if Array.length Sys.argv <> 3 then begin
    eprintf "Usage: cmpbyt <file 1> <file 2>\n";
    exit 2
  end;
  if cmpbyt Sys.argv.(1) Sys.argv.(2) then exit 0 else exit 1
