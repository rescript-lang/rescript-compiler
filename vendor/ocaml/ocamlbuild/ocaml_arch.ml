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

type 'a arch =
  | Arch_dir of string * 'a * 'a arch list
  | Arch_dir_pack of string * 'a * 'a arch list
  | Arch_file of string * 'a

let dir name contents = Arch_dir(name, (), contents)
let dir_pack name contents = Arch_dir_pack(name, (), contents)
let file name = Arch_file(name, ())

type info =
{
  current_path : string;
  include_dirs : string list;
  for_pack     : string;
}

let join_pack parent base =
  if parent = "" then base else parent ^ "." ^ base

let annotate arch =
  let rec self arch acc =
    match arch with
    | Arch_dir_pack(name, _, contents) ->
        let acc = { (acc) with for_pack = join_pack acc.for_pack name } in
        let (_, _, i, new_contents) = self_contents name contents acc in
        ([], Arch_dir_pack(name, i, List.rev new_contents))
    | Arch_dir(name, _, contents) ->
        let (current_path, include_dirs, i, new_contents) = self_contents name contents acc in
        (current_path :: include_dirs, Arch_dir(name, i, List.rev new_contents))
    | Arch_file(name, _) ->
        ([], Arch_file(name, acc))
  and self_contents name contents acc =
    let current_path = acc.current_path/name in
    let include_dirs = if current_path = "" then acc.include_dirs else current_path :: acc.include_dirs in
    let i = { (acc) with current_path = current_path; include_dirs = include_dirs } in
    let (include_dirs, new_contents) =
      List.fold_left begin fun (include_dirs, new_contents) x ->
        let j = { (i) with include_dirs = include_dirs @ i.include_dirs } in
        let (include_dirs', x') = self x j in
        (include_dirs @ include_dirs', x' :: new_contents)
      end ([], []) contents in
    (current_path, include_dirs, i, new_contents) in
  let init = { current_path = ""; include_dirs = []; for_pack = "" } in
  snd (self arch init)

let rec print print_info f =
  let rec print_contents f =
    function
    | [] -> ()
    | x :: xs -> Format.fprintf f "@ %a%a" (print print_info) x print_contents xs in
  function
  | Arch_dir(name, info, contents) ->
      Format.fprintf f "@[<v2>dir %S%a%a@]" name print_info info print_contents contents
  | Arch_dir_pack(name, info, contents) ->
      Format.fprintf f "@[<v2>dir_pack %S%a%a@]" name print_info info print_contents contents
  | Arch_file(name, info) ->
      Format.fprintf f "@[<2>file %S%a@]" name print_info info

let print_include_dirs = List.print String.print

let print_info f i =
  Format.fprintf f "@ @[<v2>{ @[<2>current_path =@ %S@];@\
                            \ @[<2>include_dirs =@ %a@];@\
                            \ @[<2>for_pack =@ %S@] }@]"
                 i.current_path print_include_dirs i.include_dirs i.for_pack

let rec iter_info f =
  function
  | Arch_dir_pack(_, i, xs) | Arch_dir(_, i, xs) ->
      f i; List.iter (iter_info f) xs
  | Arch_file(_, i) -> f i

let rec fold_info f arch acc =
  match arch with
  | Arch_dir_pack(_, i, xs) | Arch_dir(_, i, xs) ->
      List.fold_right (fold_info f) xs (f i acc)
  | Arch_file(_, i) -> f i acc

module SS = Set.Make(String)

let iter_include_dirs arch =
  let set = fold_info (fun i -> List.fold_right SS.add i.include_dirs) arch SS.empty in
  fun f -> SS.iter f set

let forpack_flags_of_pathname = ref (fun _ -> N)

let print_table print_value f table =
  Format.fprintf f "@[<hv0>{:@[<hv0>";
  Hashtbl.iter begin fun k v ->
    if k <> "" then
      Format.fprintf f "@ @[<2>%S =>@ %a@];" k print_value v;
  end table;
  Format.fprintf f "@]@ :}@]"

let print_tables f (include_dirs_table, for_pack_table) =
  Format.fprintf f "@[<2>@[<2>include_dirs_table:@ %a@];@ @[<2>for_pack_table: %a@]@]"
     (print_table (List.print String.print)) include_dirs_table
     (print_table String.print) for_pack_table

let mk_tables arch =
  let include_dirs_table = Hashtbl.create 17
  and for_pack_table = Hashtbl.create 17 in
  iter_info begin fun i ->
    Hashtbl.replace include_dirs_table i.current_path i.include_dirs;
    Hashtbl.replace for_pack_table i.current_path i.for_pack
  end arch;
  let previous_forpack_flags_of_pathname = !forpack_flags_of_pathname in
  forpack_flags_of_pathname := begin fun m ->
    let m' = Pathname.dirname m in
    try
      let for_pack = Hashtbl.find for_pack_table m' in
      if for_pack = "" then N else S[A"-for-pack"; A for_pack]
    with Not_found -> previous_forpack_flags_of_pathname m
  end;
  (* Format.eprintf "@[<2>%a@]@." print_tables (include_dirs_table, for_pack_table); *)
  (include_dirs_table, for_pack_table)

let forpack_flags_of_pathname m = !forpack_flags_of_pathname m
