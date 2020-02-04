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

(* Note the build system should check the validity of filenames espeically, it
   should not contain '-' *)
let ns_sep_char = '-'
let ns_sep = "-"

let make ?ns cunit =
  match ns with
  | None -> cunit
  | Some ns -> cunit ^ ns_sep ^ ns


(** Starting from the end, search for [ns_sep_char]. Returns the index, if
    found, or [-1] if [ns_sep_char] is not found before reaching a
    directory-separator. *)
let rec rindex_rec s i =
  if i < 0 then i
  else
    let char = String.unsafe_get s i in
    if Ext_filename.is_dir_sep char then -1
    else if char = ns_sep_char then i
    else rindex_rec s (i - 1)


(* Note we have to output uncapitalized file Name, or at least be consistent,
   since by reading cmi file on Case insensitive OS, we don't really know
   whether it is `list.cmi` or `List.cmi`, so that `require(./list.js)` or
   `require(./List.js)`. Relevant issues: #1609, #913

   #1933 when removing ns suffix, don't pass the bound of basename

   FIXME: micro-optimizaiton *)
let replace_namespace_with_extension ~name ~ext =
  let i = rindex_rec name (String.length name - 1) in
  if i < 0 then name ^ ext else String.sub name 0 i ^ ext


let try_split_module_name name =
  let len = String.length name in
  let i = rindex_rec name (len - 1) in
  if i < 0 then None
  else Some (String.sub name (i + 1) (len - i - 1), String.sub name 0 i)


type file_kind = Upper_js | Upper_bs | Little_js | Little_bs

let js_name_of_modulename s little =
  match little with
  | Little_js ->
      replace_namespace_with_extension
        ~name:(Ext_string.uncapitalize_ascii s)
        ~ext:Literals.suffix_js
  | Little_bs ->
      replace_namespace_with_extension
        ~name:(Ext_string.uncapitalize_ascii s)
        ~ext:Literals.suffix_bs_js
  | Upper_js -> replace_namespace_with_extension ~name:s ~ext:Literals.suffix_js
  | Upper_bs ->
      replace_namespace_with_extension ~name:s ~ext:Literals.suffix_bs_js


(** https://docs.npmjs.com/files/package.json

    Some rules:

    - The name must be less than or equal to 214 characters. This includes the
      scope for scoped packages.
    - The name can't start with a dot or an underscore.
    - New packages must not have uppercase letters in the name.
    - The name ends up being part of a URL, an argument on the command line, and
      a folder name. Therefore, the name can't contain any non-URL-safe
      characters.

    TODO: handle cases like '\@angular/core'. its directory structure is like:

    {[
      @angular
      |-------- core
    ]} *)
let is_valid_npm_package_name (s : string) =
  let len = String.length s in
  len <= 214 (* magic number forced by npm *)
  && len > 0
  &&
  match String.unsafe_get s 0 with
  | 'a' .. 'z' | '@' ->
      Ext_string.for_all_from s 1 (fun x ->
          match x with
          | 'a' .. 'z' | '0' .. '9' | '_' | '-' -> true
          | _ -> false)
  | _ -> false


let namespace_of_package_name (s : string) : string =
  let len = String.length s in
  let buf = Ext_buffer.create len in
  let add capital ch =
    Ext_buffer.add_char buf (if capital then Char.uppercase_ascii ch else ch)
  in
  let rec aux capital off len =
    if off >= len then ()
    else
      let ch = String.unsafe_get s off in
      match ch with
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' ->
          add capital ch;
          aux false (off + 1) len
      | '/' | '-' -> aux true (off + 1) len
      | _ -> aux capital (off + 1) len
  in
  aux true 0 len;
  Ext_buffer.contents buf
