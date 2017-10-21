(**************************************************************************)
(*                                                                        *)
(*  Ocamlgraph: a generic graph library for OCaml                         *)
(*  Copyright (C) 2004-2007                                               *)
(*  Sylvain Conchon, Jean-Christophe Filliatre and Julien Signoles        *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2, with the special exception on linking              *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(* $Id:$ *)

(* script to build graph.mli from the set of all .mli files passed on the
   command line *)

open Printf

let cout = open_out "graph.mli"

let is_cvs_line s = String.length s > 5 && String.sub s 0 6 = "(* $Id"

let copy f =
  let cin = open_in f in
  while not (is_cvs_line (input_line cin)) do () done;
  begin 
    try while true do fprintf cout "  %s\n" (input_line cin) done
    with End_of_file -> () 
  end;
  close_in cin

let () =
  for i = 1 to Array.length Sys.argv - 1 do
    let f = Sys.argv.(i) in
    if not (Sys.file_exists f) then failwith (f ^ ": no suh file");
    let m = String.capitalize (Filename.chop_suffix f ".mli") in
    fprintf cout "module %s : sig\n" m;
    copy f;
    fprintf cout "end\n\n"

  done

let () = close_out cout

