(* OCamlScript compiler
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



let js_flag = 8 (* check with ocaml compiler *)

let js_module_flag = 16 (* javascript external modules *)
(* TODO:
    check name conflicts with javascript conventions
    {[
    Ext_ident.convert "^";;
    - : string = "$caret"
    ]}
 *)
let js_object_flag = 32 (* javascript object flags *)

let is_js (i : Ident.t) = 
  i.flags land js_flag <> 0 

let is_js_module (i : Ident.t) =
  i.flags land js_module_flag <> 0 

let is_js_object (i : Ident.t) = 
  i.flags land js_object_flag <> 0 

let make_js_object (i : Ident.t) = 
  i.flags <- i.flags lor js_object_flag 
      
(* It's a js function hard coded by js api, so when printing,
   it should preserve the name 
 *)
let create_js (name : string) : Ident.t  = 
  { name = name; flags = js_flag ; stamp = 0}

let js_module_table = Hashtbl.create 31 

(* This is for a js exeternal module, we can change it when printing
   for example
   {[
   var React$1 = require('react');
   React$1.render(..)
   ]}

   Given a name, if duplicated, they should  have the same id
 *)
let create_js_module (name : string) : Ident.t = 
  let name = 
    String.concat "" @@ List.map (String.capitalize ) @@ 
    Ext_string.split name '-' in
  (* TODO: if we do such transformation, we should avoid 
      collision for example:
      react-dom 
      react--dom
      check collision later
   *)
  match Hashtbl.find js_module_table name  with 
  | exception Not_found -> 
      let v = Ident.create name in
      let ans = { v with flags = js_module_flag} in 
      Hashtbl.add js_module_table name ans;
      ans
  | v -> v 

let create = Ident.create

let gen_js ?(name="$js") () = create name 

let reserved_words = 
  [
    (* keywork *)
    "break";
    "case"; "catch"; "continue";
    "debugger";"default";"delete";"do";
    "else";
    "finally";"for";"function";
    "if"; "in";"instanceof";
    "new";
    "return";
    "switch";
    "this"; "throw"; "try"; "typeof";
    "var"; "void"; "while"; "with";

    (* reserved in ECMAScript 5 *)
    "class"; "enum"; "export"; "extends"; "import"; "super";

    "implements";"interface";
    "let";
    "package";"private";"protected";"public";
    "static";
    "yield";

    (* other *)
    "null";
    "true";
    "false";
    "NaN";


    "undefined";
    "this";

    (* also reserved in ECMAScript 3 *)
    "abstract"; "boolean"; "byte"; "char"; "const"; "double";
    "final"; "float"; "goto"; "int"; "long"; "native"; "short";
    "synchronized"; "throws"; "transient"; "volatile";

    (* also reserved in ECMAScript 6 *)
    "await";
   
   "event";
   "location";
   "window";
   "document";
   "eval";
   "navigator";
   (* "self"; *)
   
   "Array";
   "Date";
   "Math";
   "JSON";
   "Object";
   "RegExp";
   "String";
   "Boolean";
   "Number";

   "Map"; (* es6*)
   "Set";

   "Infinity";
   "isFinite";
   
   "ActiveXObject";
   "XMLHttpRequest";
   "XDomainRequest";
   
   "DOMException";
   "Error";
   "SyntaxError";
   "arguments";
   
   "decodeURI";
   "decodeURIComponent";
   "encodeURI";
   "encodeURIComponent";
   "escape";
   "unescape";

   "isNaN";
   "parseFloat";
   "parseInt";
   
   (** reserved for commonjs *)   
   "require";
   "exports";
   "module"
]

let reserved_map = 
  List.fold_left (fun acc x -> String_set.add x acc) String_set.empty 
    reserved_words

(* TODO:
    check name conflicts with javascript conventions
    {[
    Ext_ident.convert "^";;
    - : string = "$caret"
    ]}
 *)
let convert (name : string) = 
   let module E = struct exception Not_normal_letter of int end in
   let len = String.length name  in
   if String_set.mem name reserved_map then "$$" ^ name 
   else 
   try
   for i  = 0 to len - 1 do 
   let c = String.unsafe_get name i in
   if not ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_' || c = '$' ) then
       raise (E.Not_normal_letter i)
    else ()
    done;
     name
  with E.Not_normal_letter i ->
   String.sub name 0 i ^ 
   (let buffer = Buffer.create len in 
   for j = i to  len - 1 do 
     let c = String.unsafe_get name j in
     match c with 
     | '*' -> Buffer.add_string buffer "$star"
     | '\'' -> Buffer.add_string buffer "$prime"
     | '!' -> Buffer.add_string buffer "$bang"
     | '>' -> Buffer.add_string buffer "$great"
     | '<' -> Buffer.add_string buffer "$less"
     | '=' -> Buffer.add_string buffer "$eq"
     | '+' -> Buffer.add_string buffer "$plus"
     | '-' -> Buffer.add_string buffer "$neg"
     | '@' -> Buffer.add_string buffer "$at"
     | '^' -> Buffer.add_string buffer "$caret"
     | '/' -> Buffer.add_string buffer "$slash"
     | '.' -> Buffer.add_string buffer "$dot"
     | 'a'..'z' | 'A'..'Z'| '_'|'$' |'0'..'9'-> Buffer.add_char buffer  c
     | _ -> Buffer.add_string buffer "$unknown"
   done; Buffer.contents buffer)

(* It is currently made a persistent ident to avoid fresh ids 
    which would result in different signature files
    - other solution: use lazy values
*)
let make_unused () = create "_"

let is_unused_ident i = Ident.name i = "_"

let reset () = 
  begin
    Hashtbl.clear js_module_table
  end
