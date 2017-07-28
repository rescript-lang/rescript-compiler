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
open Command
open Pathname.Operators
let () = Log.level := -1000

let usage () =
  Format.eprintf "Usage: %s <preprocess-command> <preprocess-args>@." Sys.argv.(0);
  exit 4

let () = if Array.length Sys.argv < 2 then usage ()

let args = List.tl (Array.to_list Sys.argv)

let buf = Buffer.create 2048

let digest_file file =
  Buffer.add_string buf (Digest.file file)
let digest_string string =
  Buffer.add_string buf (Digest.string string)

let search_in_path x =
  if Sys.file_exists x then x else
  try search_in_path x
  with Not_found -> (Format.eprintf "Command not found %s@." x; exit 3)

let cmd =
  match args with
  | ocamlrun :: x :: _ when String.contains_string ocamlrun 0 "ocamlrun" <> None ->
      digest_file (search_in_path ocamlrun); x
  | x :: _ -> x
  | _ -> usage ()

let output = ref ""

let () = digest_file (search_in_path cmd)

let rec loop =
  function
  | [] -> Digest.string (Buffer.contents buf)
  | ("-impl"|"-intf") :: x :: xs ->
      digest_string x; digest_file x; loop xs
  | "-o" :: x :: xs ->
      output := x; loop xs
  | x :: xs ->
      let ext = Pathname.get_extension x in
      digest_string x;
      (match ext with
      | "cmo" | "cma" | "ml" | "mli" -> digest_file x
      | _ -> ());
      loop xs

let digest = loop args;;

let cache_dir = "/tmp/ppcache";; (* FIXME *)

let () = Shell.mkdir_p cache_dir;;

let path = cache_dir/(Digest.to_hex digest);;

let cat path = with_input_file ~bin:true path (fun ic -> copy_chan ic stdout);;

if sys_file_exists path then
  if !output = "" then
    cat path
  else
    Shell.cp path !output
else
  let cmd = atomize args in
  if !output = "" then begin
    let tmp = path^".tmp" in
    Command.execute (Cmd(S[cmd; Sh ">"; A tmp]));
    Shell.mv tmp path;
    cat path
  end else begin
    Command.execute (Cmd cmd);
    Shell.cp !output path
  end
