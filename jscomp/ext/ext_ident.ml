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








let js_flag = 0b1_000 (* check with ocaml compiler *)

(* let js_module_flag = 0b10_000 (\* javascript external modules *\) *)
(* TODO:
    check name conflicts with javascript conventions
   {[
     Ext_ident.convert "^";;
     - : string = "$caret"
   ]}
*)
let js_object_flag = 0b100_000 (* javascript object flags *)

let is_js (i : Ident.t) = 
  i.flags land js_flag <> 0 

let is_js_or_global (i : Ident.t) = 
  i.flags land (8 lor 1) <> 0 


let is_js_object (i : Ident.t) = 
  i.flags land js_object_flag <> 0 

let make_js_object (i : Ident.t) = 
  i.flags <- i.flags lor js_object_flag 

(* It's a js function hard coded by js api, so when printing,
   it should preserve the name 
*)
let create_js (name : string) : Ident.t  = 
  { name = name; flags = js_flag ; stamp = 0}

let create = Ident.create

(* FIXME: no need for `$' operator *)
let create_tmp ?(name=Literals.tmp) () = create name 


let js_module_table : Ident.t String_hashtbl.t = String_hashtbl.create 31 

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
    String.concat "" @@ Ext_list.map (Ext_string.capitalize_ascii ) @@ 
    Ext_string.split name '-' in
  (* TODO: if we do such transformation, we should avoid       collision for example:
      react-dom 
      react--dom
      check collision later
  *)
  match String_hashtbl.find_exn js_module_table name  with 
  | exception Not_found -> 
    let ans = Ident.create name in
    (* let ans = { v with flags = js_module_flag} in  *)
    String_hashtbl.add js_module_table name ans;
    ans
  | v -> (* v *) Ident.rename v  


let reserved_words = 
  [|
    (* keywork *)
    "break";
    "case"; "catch"; "continue";
    "debugger";"default";"delete";"do";
    "else";
    "finally";"for";"function";
    "if"; "then"; "in";"instanceof";
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
    "synchronized"; 
    (* "throws";  *)
    (* seems to be fine, like nodejs [assert.throws] *)
    "transient"; "volatile";

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

    (** reserved for commonjs and NodeJS globals*)   
    "require";
    "exports";
    "module";
    "clearImmediate";
    "clearInterval";
    "clearTimeout";
    "console";
    "global";
    "process";
    "require";
    "setImmediate";
    "setInterval";
    "setTimeout";
    "__dirname";
    "__filename";
    "__esModule"
  |]

let reserved_map = 
  let len = Array.length reserved_words in 
  let set =  String_hash_set.create 1024 in (* large hash set for perfect hashing *)
  for i = 0 to len - 1 do 
    String_hash_set.add set reserved_words.(i);
  done ;
  set 



exception Not_normal_letter of int 
let name_mangle name = 

  let len = String.length name  in
  try
    for i  = 0 to len - 1 do 
      match String.unsafe_get name i with 
      | 'a' .. 'z' | 'A' .. 'Z'
      | '0' .. '9' | '_' | '$'
        -> ()
      | _ -> raise (Not_normal_letter i)
    done;
    name (* Normal letter *)
  with 
  | Not_normal_letter 0 ->

    let buffer = Buffer.create len in 
    for j = 0 to  len - 1 do 
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
      | '|' -> Buffer.add_string buffer "$pipe"
      | '.' -> Buffer.add_string buffer "$dot"
      | '%' -> Buffer.add_string buffer "$percent"
      | '~' -> Buffer.add_string buffer "$tilde"
      | '#' -> Buffer.add_string buffer "$hash"
      | 'a'..'z' | 'A'..'Z'| '_' 
      | '$'
      | '0'..'9'-> Buffer.add_char buffer  c
      | _ -> Buffer.add_string buffer "$unknown"
    done; Buffer.contents buffer
  | Not_normal_letter i -> 
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
       | '-' -> Buffer.add_string buffer "$" 
        (* Note ocaml compiler also has [self-] *)
       | '@' -> Buffer.add_string buffer "$at"
       | '^' -> Buffer.add_string buffer "$caret"
       | '/' -> Buffer.add_string buffer "$slash"
       | '|' -> Buffer.add_string buffer "$pipe"
       | '.' -> Buffer.add_string buffer "$dot"
       | '%' -> Buffer.add_string buffer "$percent"
       | '~' -> Buffer.add_string buffer "$tilde"
       | '#' -> Buffer.add_string buffer "$hash"
       | '$' -> Buffer.add_string buffer "$dollar"
       | 'a'..'z' | 'A'..'Z'| '_'        
       | '0'..'9'-> Buffer.add_char buffer  c
       | _ -> Buffer.add_string buffer "$unknown"
     done; Buffer.contents buffer)
(* TODO:
    check name conflicts with javascript conventions
   {[
     Ext_ident.convert "^";;
     - : string = "$caret"
   ]}
   [convert name] if [name] is a js keyword,add "$$"
   otherwise do the name mangling to make sure ocaml identifier it is 
   a valid js identifier
*)
let convert (name : string) = 
  if  String_hash_set.mem reserved_map name  then "$$" ^ name 
  else name_mangle name 

(** keyword could be used in property *)

(* It is currently made a persistent ident to avoid fresh ids 
    which would result in different signature files
    - other solution: use lazy values
*)
let make_unused () = create "_"



let reset () = 
  String_hashtbl.clear js_module_table


let undefined = create_js "undefined"
let nil = create_js "null"

(* Has to be total order, [x < y] 
   and [x > y] should be consistent
   flags are not relevant here 
*)
let compare (x : Ident.t ) ( y : Ident.t) = 
  let u = x.stamp - y.stamp in
  if u = 0 then 
    Ext_string.compare x.name y.name 
  else u 

let equal ( x : Ident.t) ( y : Ident.t) = 
  if x.stamp <> 0 then x.stamp = y.stamp
  else y.stamp = 0 && x.name = y.name

