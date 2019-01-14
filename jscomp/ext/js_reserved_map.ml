(* Copyright (C) 2019-Present Authors of BuckleScript
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
    "Buffer"; (* Node *)
    "Map"; (* es6*)
    "Set";
    "Promise";
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


let is_reserved s = 
  String_hash_set.mem reserved_map s 