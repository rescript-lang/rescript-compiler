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
open Format
open Log

type t = string

include Filename

let print_strings = List.print String.print

let concat = filename_concat

let compare (x:t) y = compare x y

let print = pp_print_string

let mk s = s

let pwd = Sys.getcwd ()

let add_extension ext x = x ^ "." ^ ext

let check_extension x ext =
  let lx = String.length x and lext = String.length ext in
  lx > lext + 1 && x.[lx - lext - 1] = '.' && String.is_suffix x ext

module Operators = struct
  let ( / ) = concat
  let ( -.- ) file ext = add_extension ext file
end
open Operators

let equal x y = x = y

let to_string x = x

let is_link = Shell.is_link
let readlink = Shell.readlink
let is_directory x =
  try (My_unix.stat x).My_unix.stat_file_kind = My_unix.FK_dir
  with Sys_error _ -> false
let readdir x = Outcome.good (sys_readdir x)

let dir_seps = ['/';'\\'] (* FIXME add more *)
let not_normal_form_re = Glob.parse "<**/{,.,..}/**>"

let parent x = concat parent_dir_name x

let split p =
  let rec go p acc =
    let dir = dirname p in
    if dir = p then dir, acc
    else go dir (basename p :: acc)
  in go p []

let join root paths =
  let root = if root = current_dir_name then "" else root in
  List.fold_left (/) root paths

let _H1 = assert (current_dir_name = ".")
let _H2 = assert (parent_dir_name = "..")

(* Use H1, H2 *)
let rec normalize_list = function
  | [] -> []
  | "." :: xs -> normalize_list xs
  | ".." :: _ -> failwith "Pathname.normalize_list: .. is forbidden here"
  | _ :: ".." :: xs -> normalize_list xs
  | x :: xs -> x :: normalize_list xs

let normalize x =
  if Glob.eval not_normal_form_re x then
    let root, paths = split x in
    join root (normalize_list paths)
  else x

(* [is_prefix x y] is [x] a pathname prefix of [y] *)
let is_prefix x y =
  let lx = String.length x and ly = String.length y in
  if lx = ly then x = (String.before y lx)
  else if lx < ly then x = (String.before y lx) && List.mem y.[lx] dir_seps
  else false

let link_to_dir p dir = is_link p && is_prefix dir (readlink p)

let remove_extension x =
  try chop_extension x
  with Invalid_argument _ -> x
let get_extension x =
  try
    let pos = String.rindex x '.' in
    String.after x (pos + 1)
  with Not_found -> ""
let update_extension ext x =
  add_extension ext (chop_extension x)

let chop_extensions x =
  let dirname = dirname x and basename = basename x in
  try
    let pos = String.index basename '.' in
    dirname / (String.before basename pos)
  with Not_found -> invalid_arg "chop_extensions: no extensions"
let remove_extensions x =
  try chop_extensions x
  with Invalid_argument _ -> x
let get_extensions x =
  let basename = basename x in
  try
    let pos = String.index basename '.' in
    String.after basename (pos + 1)
  with Not_found -> ""
let update_extensions ext x =
  add_extension ext (chop_extensions x)

let exists = sys_file_exists

let copy = Shell.cp
let remove = Shell.rm
let try_remove x = if exists x then Shell.rm x
let read = read_file

let with_input_file = with_input_file

let with_output_file = with_output_file

let print_path_list = List.print print

let context_table = Hashtbl.create 107

let rec include_dirs_of dir =
  try Hashtbl.find context_table dir
  with Not_found -> dir :: List.filter (fun dir' -> dir <> dir') !Options.include_dirs

(*
let include_dirs_of s =
  let res = include_dirs_of s in
  let () = dprintf 0 "include_dirs_of %S ->@ %a" s (List.print print) res
  in res
*)

let define_context dir context =
  let dir = if dir = "" then current_dir_name else dir in
  Hashtbl.replace context_table dir& List.union context& include_dirs_of dir

let same_contents x y = Digest.file x = Digest.file y
