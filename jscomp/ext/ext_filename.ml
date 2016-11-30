(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)








(** Used when produce node compatible paths *)
let node_sep = "/"
let node_parent = ".."
let node_current = "."

type t = 
  [ `File of string 
  | `Dir of string ]

let cwd = lazy (Sys.getcwd ())

let (//) = Filename.concat 

let combine path1 path2 =
  if path1 = "" then
    path2
  else if path2 = "" then path1
  else 
  if Filename.is_relative path2 then
    path1// path2 
  else
    path2

(* Note that [.//] is the same as [./] *)
let path_as_directory x =
  if x = "" then x
  else
  if Ext_string.ends_with x  Filename.dir_sep then
    x 
  else 
    x ^ Filename.dir_sep

let absolute_path s = 
  let process s = 
    let s = 
      if Filename.is_relative s then
        Lazy.force cwd // s 
      else s in
    (* Now simplify . and .. components *)
    let rec aux s =
      let base,dir  = Filename.basename s, Filename.dirname s  in
      if dir = s then dir
      else if base = Filename.current_dir_name then aux dir
      else if base = Filename.parent_dir_name then Filename.dirname (aux dir)
      else aux dir // base
    in aux s  in 
  process s 


let chop_extension ?(loc="") name =
  try Filename.chop_extension name 
  with Invalid_argument _ -> 
    Ext_pervasives.invalid_argf 
      "Filename.chop_extension ( %s : %s )"  loc name

let chop_extension_if_any fname =
  try Filename.chop_extension fname with Invalid_argument _ -> fname





let os_path_separator_char = String.unsafe_get Filename.dir_sep 0 


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
let relative_path file_or_dir_1 file_or_dir_2 = 
  let sep_char = os_path_separator_char in
  let relevant_dir1 = 
    (match file_or_dir_1 with 
     | `Dir x -> x 
     | `File file1 ->  Filename.dirname file1) in
  let relevant_dir2 = 
    (match file_or_dir_2 with 
     |`Dir x -> x 
     |`File file2 -> Filename.dirname file2 ) in
  let dir1 = Ext_string.split relevant_dir1 sep_char   in
  let dir2 = Ext_string.split relevant_dir2 sep_char  in
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

    [file1] is currently compilation file 
    [file2] is the dependency
*)
let node_relative_path (file1 : t) 
    (`File file2 as dep_file : [`File of string]) = 
  let v = Ext_string.find  file2 ~sub:Literals.node_modules in 
  let len = String.length file2 in 
  if v >= 0 then
    let rec skip  i =       
      if i >= len then
        Ext_pervasives.failwithf ~loc:__LOC__ "invalid path: %s"  file2
      else 
        (* https://en.wikipedia.org/wiki/Path_(computing))
           most path separator are a single char 
        *)
        let curr_char = String.unsafe_get file2 i  in 
        if curr_char = os_path_separator_char || curr_char = '.' then 
          skip (i + 1) 
        else i
        (*
          TODO: we need do more than this suppose user 
          input can be
           {[
             "xxxghsoghos/ghsoghso/node_modules/../buckle-stdlib/list.js"
           ]}
           This seems weird though
        *)
    in 
    Ext_string.tail_from file2
      (skip (v + Literals.node_modules_length)) 
  else 
    relative_path 
      (  match dep_file with 
         | `File x -> `File (absolute_path x)
         | `Dir x -> `Dir (absolute_path x))

      (match file1 with 
       | `File x -> `File (absolute_path x)
       | `Dir x -> `Dir(absolute_path x))
    ^ node_sep ^
    chop_extension_if_any (Filename.basename file2)





let find_package_json_dir cwd  = 
  let rec aux cwd  = 
    if Sys.file_exists (cwd // Literals.package_json) then cwd
    else 
      let cwd' = Filename.dirname cwd in 
      if String.length cwd' < String.length cwd then  
        aux cwd'
      else 
        Ext_pervasives.failwithf 
          ~loc:__LOC__
          "package.json not found from %s" cwd
  in
  aux cwd 

let package_dir = lazy (find_package_json_dir (Lazy.force cwd))


let rec no_slash x i len = 
  i >= len  || 
  (String.unsafe_get x i <> '/' && no_slash x (i + 1)  len)

let replace_backward_slash (x : string)=
  let len = String.length x in
  if no_slash x 0 len then x 
  else  
    String.map (function 
        |'\\'-> '/'
        | x -> x) x


let replace_slash_backward (x : string ) = 
  let len = String.length x in 
  if no_slash x 0 len then x 
  else 
    String.map (function 
        | '/' -> '\\'
        | x -> x ) x 

let module_name_of_file file =
  String.capitalize 
    (Filename.chop_extension @@ Filename.basename file)  

let module_name_of_file_if_any file = 
  String.capitalize 
    (chop_extension_if_any @@ Filename.basename file)  


(** For win32 or case insensitve OS 
    [".cmj"] is the same as [".CMJ"]
*)
(* let has_exact_suffix_then_chop fname suf =  *)

let combine p1 p2 = 
  if p1 = "" || p1 = Filename.current_dir_name then p2 else 
  if p2 = "" || p2 = Filename.current_dir_name then p1 
  else 
  if Filename.is_relative p2 then 
    Filename.concat p1 p2 
  else p2 



(**
   {[
     split_aux "//ghosg//ghsogh/";;
     - : string * string list = ("/", ["ghosg"; "ghsogh"])
   ]}
*)
let split_aux p =
  let rec go p acc =
    let dir = Filename.dirname p in
    if dir = p then dir, acc
    else go dir (Filename.basename p :: acc)
  in go p []

(** 
   TODO: optimization
   if [from] and [to] resolve to the same path, a zero-length string is returned 
*)
let rel_normalized_absolute_path from to_ =
  let root1, paths1 = split_aux from in 
  let root2, paths2 = split_aux to_ in 
  if root1 <> root2 then root2 else
    let rec go xss yss =
      match xss, yss with 
      | x::xs, y::ys -> 
        if x = y then go xs ys 
        else 
          let start = 
            List.fold_left (fun acc _ -> acc // ".." ) ".." xs in 
          List.fold_left (fun acc v -> acc // v) start yss
      | [], [] -> ""
      | [], y::ys -> List.fold_left (fun acc x -> acc // x) y ys
      | x::xs, [] ->
        List.fold_left (fun acc _ -> acc // ".." ) ".." xs in
    go paths1 paths2



module IntInt_array = Resize_array.Make(struct type t = int * int 
  let null = (0, -1) end)

let dir_sep_char = Filename.dir_sep.[0]

let is_slash chr = chr = '/' || chr = dir_sep_char

let is_dot chr = chr = '.'

let get_root path = 
  if String.length path = 0 then "." 
  else if dir_sep_char = '/' && path.[0] = dir_sep_char then "/"
  else if dir_sep_char = '\\' && String.length path >= 2 && path.[1] = ':' then 
    String.sub path 0 2
  else "."

let has_absolute_root str = 
  get_root str <> "."

let split_index path = 
  let idxs = IntInt_array.make (1 + String.length path / 2) in
  let rec splitter idx len start =
    begin
      if idx >= String.length path then (
        (if len > 0 then IntInt_array.push idxs (start, idx-1));
        idxs
      )
      else
        begin
          if len = 0 then (
            if is_slash path.[idx] then splitter (idx+1) 0 0
            else splitter (idx+1) 1 idx)
          else (
            if is_slash path.[idx] then 
              (IntInt_array.push idxs (start, idx); 
               splitter (idx+1) 0 0)
            else splitter (idx+1) (len+1) start
          )
        end
    end
  in
  splitter 0 0 0

(*TODO: could be hgighly optimized later 
  {[
    normalize_absolute_path "/gsho/./..";;

    normalize_absolute_path "/a/b/../c../d/e/f";;

    normalize_absolute_path "/gsho/./..";;

    normalize_absolute_path "/gsho/./../..";;

    normalize_absolute_path "/a/b/c/d";;

    normalize_absolute_path "/a/b/c/d/";;

    normalize_absolute_path "/a/";;

    normalize_absolute_path "/a";;
  ]}
*)
let normalize_absolute_path path =
  let root = get_root path in 
  let rootl = String.length root in
  let len (st,ed) = ed - st + 1 in
  let sdot (st,ed) = 
    (2 = len (st,ed) && is_dot path.[st] && is_slash path.[st+1])
    || (1 = len (st,ed) && is_dot path.[st]) in
  let ddot (st,ed) = 
    (3 = len (st,ed) && is_dot path.[st] && is_dot path.[st+1] 
    && is_slash path.[st+2]) 
    || (2 = len (st,ed) && is_dot path.[st] && is_dot path.[st+1]) in
  (* Remove single dot immediately *)
  let idxs = split_index path 
  |> IntInt_array.filter (fun elt -> not (sdot elt)) in
  let i = ref 0 in 
  (* Remove double dots *)
  while !i < IntInt_array.length idxs do
  begin
    let at_i = IntInt_array.get idxs !i in 
    if ddot at_i then (
      IntInt_array.delete idxs !i;
      if !i > 0 && IntInt_array.length idxs > 0 then 
        (IntInt_array.delete idxs (!i-1); decr i);
    )
    else incr i
  end
  done;
  (* Create the string *)
  i := 0;
  let fbytes = IntInt_array.fold_left (fun accum elt -> accum + len elt) 0 idxs 
    |> Bytes.create in
  IntInt_array.iter
    (fun (st,ed) -> 
       let length = len (st,ed) in
       Bytes.blit_string path st fbytes !i length;
       i := !i + length)
    idxs;
  (* Rest is root calculation *)
  let fbl = Bytes.length fbytes in
  let fbytes = Bytes.to_string fbytes in 
  let nroot = if root = Filename.dir_sep then root else root^Filename.dir_sep in
  if fbl = 0 then root
  else if fbl < rootl then nroot ^ fbytes
  else if fbl = rootl then (if root = fbytes then fbytes else nroot ^ fbytes)
  else (
    if has_absolute_root root then 
      (if root = String.sub fbytes 0 rootl then fbytes else nroot ^ fbytes)
    else (
      if root ^ Filename.dir_sep = String.sub fbytes 0 (rootl+1) then fbytes
      else nroot ^ fbytes
    )
  )

let get_extension x =
  try
    let pos = String.rindex x '.' in
    Ext_string.tail_from x pos
  with Not_found -> ""


