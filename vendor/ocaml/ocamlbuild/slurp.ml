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


(* Original author: Berke Durak *)
(* Slurp *)
open My_std
open Outcome

type 'a entry =
  | Dir of string * string * My_unix.stats Lazy.t * 'a * 'a entry list Lazy.t
  | File of string * string * My_unix.stats Lazy.t * 'a
  | Error of exn
  | Nothing

let (/) = filename_concat

let rec filter predicate = function
  | Dir(path, name, st, attr, entries) ->
      if predicate path name attr then
        Dir(path, name, st, attr, lazy (List.map (filter predicate) !*entries))
      else
        Nothing
  | File(path, name, _, attr) as f ->
      if predicate path name attr then
        f
      else
        Nothing
  | Nothing -> Nothing
  | Error _ as e -> e

let real_slurp path =
  let cwd = Sys.getcwd () in
  let abs x = if Filename.is_implicit x || Filename.is_relative x then cwd/x else x in
  let visited = Hashtbl.create 1024 in
  let rec scandir path names =
    let (file_acc, dir_acc) =
      Array.fold_left begin fun ((file_acc, dir_acc) as acc) name ->
        match do_entry true path name with
        | None -> acc
        | Some((Dir _|Error _) as entry) -> (file_acc, entry :: dir_acc)
        | Some((File _) as entry) -> (entry :: file_acc, dir_acc)
        | Some Nothing -> acc
      end
      ([], [])
      names
    in
    file_acc @ dir_acc
  and do_entry link_mode path name =
    let fn = path/name in
    let absfn = abs fn in
    match
      try
        Good(if link_mode then My_unix.lstat absfn else My_unix.stat absfn)
      with
      | x -> Bad x
    with
    | Bad x -> Some(Error x)
    | Good st ->
      let key = st.My_unix.stat_key in
      if try Hashtbl.find visited key with Not_found -> false
      then None
      else
        begin
          Hashtbl.add visited key true;
          let res =
            match st.My_unix.stat_file_kind with
            | My_unix.FK_link ->
                let fn' = My_unix.readlink absfn in
                if sys_file_exists (abs fn') then
                  do_entry false path name
                else
                  Some(File(path, name, lazy st, ()))
            | My_unix.FK_dir ->
                (match sys_readdir absfn with
                | Good names -> Some(Dir(path, name, lazy st, (), lazy (scandir fn names)))
                | Bad exn -> Some(Error exn))
            | My_unix.FK_other -> None
            | My_unix.FK_file -> Some(File(path, name, lazy st, ())) in
          Hashtbl.replace visited key false;
          res
        end
  in
  match do_entry true "" path with
  | None -> raise Not_found
  | Some entry -> entry

let split path =
  let rec aux path =
    if path = Filename.current_dir_name then []
    else (Filename.basename path) :: aux (Filename.dirname path)
  in List.rev (aux path)

let rec join =
  function
  | [] -> assert false
  | [x] -> x
  | x :: xs -> x/(join xs)

let rec add root path entries =
  match path, entries with
  | [], _ -> entries
  | xpath :: xspath, (Dir(dpath, dname, dst, dattr, dentries) as d) :: entries ->
      if xpath = dname then
        Dir(dpath, dname, dst, dattr, lazy (add (root/xpath) xspath !*dentries)) :: entries
      else d :: add root path entries
  | [xpath], [] ->
      [File(root, xpath, lazy (My_unix.stat (root/xpath)), ())]
  | xpath :: xspath, [] ->
      [Dir(root/(join xspath), xpath,
           lazy (My_unix.stat (root/(join path))), (),
           lazy (add (root/xpath) xspath []))]
  | _, Nothing :: entries -> add root path entries
  | _, Error _ :: _ -> entries
  | [xpath], (File(_, fname, _, _) as f) :: entries' ->
      if xpath = fname then entries
      else f :: add root path entries'
  | xpath :: xspath, (File(fpath, fname, fst, fattr) as f) :: entries' ->
      if xpath = fname then
        Dir(fpath, fname, fst, fattr, lazy (add (root/xpath) xspath [])) :: entries'
      else f :: add root path entries'

let slurp_with_find path =
  let find_cmd = try Sys.getenv "OCAMLBUILD_FIND" with _ -> "find" in
  let lines =
    My_unix.run_and_open (Printf.sprintf "%s %s" find_cmd (Filename.quote path)) begin fun ic ->
      let acc = ref [] in
      try while true do acc := input_line ic :: !acc done; []
      with End_of_file -> !acc
    end in
  let res =
    List.fold_right begin fun line acc ->
      add path (split line) acc
    end lines [] in
  match res with
  | [] -> Nothing
  | [entry] -> entry
  | entries -> Dir(path, Filename.basename path, lazy (My_unix.stat path), (), lazy entries)

let slurp x = if !*My_unix.is_degraded then slurp_with_find x else real_slurp x

let rec print print_attr f entry =
  match entry with
  | Dir(path, name, _, attr, entries) ->
      Format.fprintf f "@[<2>Dir(%S,@ %S,@ _,@ %a,@ %a)@]"
        path name print_attr attr (List.print (print print_attr)) !*entries
  | File(path, name, _, attr) ->
      Format.fprintf f "@[<2>File(%S,@ %S,@ _,@ %a)@]" path name print_attr attr
  | Nothing ->
      Format.fprintf f "Nothing"
  | Error(_) ->
      Format.fprintf f "Error(_)"

let rec fold f entry acc =
  match entry with
  | Dir(path, name, _, attr, contents) ->
      f path name attr (List.fold_right (fold f) !*contents acc)
  | File(path, name, _, attr) ->
      f path name attr acc
  | Nothing | Error _ -> acc

let map f entry =
  let rec self entry =
    match entry with
    | Dir(path, name, st, attr, contents) ->
        Dir(path, name, st, f path name attr, lazy (List.map self !*contents))
    | File(path, name, st, attr) ->
        File(path, name, st, f path name attr)
    | Nothing -> Nothing
    | Error e -> Error e
  in self entry

let rec force =
  function
  | Dir(_, _, st, _, contents) ->
      let _ = !*st in List.iter force !*contents
  | File(_, _, st, _) ->
      ignore !*st
  | Nothing | Error _ -> ()
