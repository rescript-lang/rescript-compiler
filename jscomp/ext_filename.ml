(* BuckleScript compiler
 * Copyright (C) 2015-2016 Bloomberg Finance L.P.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

(* Author: Hongbo Zhang  *)



(** Used when produce node compatible paths *)
let node_sep = "/"
let node_parent = ".."
let node_current = "."

let absolute_path s = 
  let s = if Filename.is_relative s then Filename.concat (Sys.getcwd ()) s else s in
  (* Now simplify . and .. components *)
  let rec aux s =
    let base = Filename.basename s in
    let dir = Filename.dirname s in
    if dir = s then dir
    else if base = Filename.current_dir_name then aux dir
    else if base = Filename.parent_dir_name then Filename.dirname (aux dir)
    else Filename.concat (aux dir) base
  in
  aux s

let chop_extension ?(loc="") name =
  try Filename.chop_extension name 
  with Invalid_argument _ -> 
    invalid_arg ("Filename.chop_extension (" ^ loc ^ ":" ^ name ^ ")")

let try_chop_extension s = try Filename.chop_extension s with _ -> s 

(** example
    {[
    "/bb/mbigc/mbig2899/bgit/bucklescript/jscomp/stdlib/external/pervasives.cmj"
    "/bb/mbigc/mbig2899/bgit/bucklescript/jscomp/stdlib/ocaml_array.ml"
    ]}

    The other way
    {[
    
    "/bb/mbigc/mbig2899/bgit/bucklescript/jscomp/stdlib/ocaml_array.ml"
    "/bb/mbigc/mbig2899/bgit/bucklescript/jscomp/stdlib/external/pervasives.cmj"
    ]}
    {[
    "/bb/mbigc/mbig2899/bgit/bucklescript/jscomp/stdlib//ocaml_array.ml"
    ]}
    {[
    /a/b
    /c/d
    ]}
 *)
let relative_path file1 file2 = 
  let dir1 = Ext_string.split (Filename.dirname file1) (Filename.dir_sep.[0])  in
  let dir2 = Ext_string.split (Filename.dirname file2) (Filename.dir_sep.[0])  in
  let rec go (dir1 : string list) (dir2 : string list) = 
    match dir1, dir2 with 
    | x::xs , y :: ys when x = y
      -> go xs ys 
    | _, _
      -> 
        List.map (fun _ -> node_parent) dir2 @ dir1 
  in
  match go dir1 dir2 with
  | (x :: _ ) as ys when x = node_parent -> 
      String.concat node_sep ys
  | ys -> 
      String.concat node_sep  @@ node_current :: ys



(** path2: a/b 
    path1: a 
    result:  ./b 
    TODO: [Filename.concat] with care
 *)
let node_relative_path path1 path2 = 
  
    (relative_path 
       (try_chop_extension (absolute_path path2))
       (try_chop_extension (absolute_path path1))
    ) ^ node_sep ^
    (try_chop_extension (Filename.basename path2))
