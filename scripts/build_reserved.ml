(* Copyright (C) 2019-Present Hongbo Zhang, Authors of ReScript
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

module SSet = Set.Make (String)

(* Words that can never be identifier's name
   See https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Lexical_grammar#reserved_words
*)
let js_keywords =
  SSet.of_list
    [
      "break";
      "case";
      "catch";
      "class";
      "const";
      "continue";
      "debugger";
      "default";
      "delete";
      "do";
      "else";
      "export";
      "extends";
      "false";
      "finally";
      "for";
      "function";
      "if";
      "import";
      "in";
      "instanceof";
      "new";
      "null";
      "return";
      "super";
      "switch";
      "this";
      "throw";
      "true";
      "try";
      "typeof";
      "var";
      "void";
      "while";
      "with";
      (* The following are also reserved in strict context, including ESM *)
      "let";
      "static";
      "yield";
      (* `await` is reserved in async context, including ESM *)
      "await";
      (* Future reserved words *)
      "enum";
      "implements";
      "interface";
      "package";
      "private";
      "protected";
      "public";
    ]

(* Identifiers with special meanings
   See https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Lexical_grammar#identifiers_with_special_meanings

   They can have different meanings depending on the context when used as identifier names, so it should be done carefully.
*)
let js_special_words =
  SSet.of_list
    [
      "arguments";
      "as";
      "async";
      "eval";
      (* However, some of these are actually used with no problems today.
         Preventing this can be annoying. *)
      (*
      "from";
      "get";
      "of";
      "set";
      *)
    ]

(* Other identifier names should be care about *)
let reserved_words =
  SSet.of_list
    [
      (* Reserved for common globals *)
      "undefined";
      "self";
      "globalThis";
      "console";
      "setTimeout";
      "setInterval";
      "clearTimeout";
      "clearInterval";
      "decodeURI";
      "decodeURIComponent";
      "encodeURI";
      "encodeURIComponent";
      "escape";
      "unescape";
      "fetch";
      "isNaN";
      "isFinite";
      "parseFloat";
      "parseInt";
      (* Reserved for common DOM globals *)
      "event";
      "window";
      "document";
      "location";
      "navigator";
      (* Reserved for common Node.js globals *)
      "Buffer";
      "setImmediate";
      "clearImmediate";
      "global";
      "process";
      "require";
      "module";
      "exports";
      "__dirname";
      "__filename";
      "__esModule";
      (* Bun global obj *)
      "Bun";
      (* Deno global obj *)
      "Deno";
    ]

let get_predefined_words (fn : string) =
  let v = ref SSet.empty in
  let in_chan = open_in_bin fn in
  (try
     while true do
       let new_word = input_line in_chan in
       if String.length new_word <> 0 then v := SSet.add new_word !v
     done
   with End_of_file -> ());
  !v

let license =
  {|(* Copyright (C) 2019-Present Hongbo Zhang, Authors of ReScript
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

|}

let binary_search =
  {|type element = string 

let rec binarySearchAux (arr : element array) (lo : int) (hi : int) key : bool =
  let mid = (lo + hi)/2 in
  let midVal = Array.unsafe_get arr mid in
  if key = midVal then true
  else if key < midVal then (* a[lo] =< key < a[mid] <= a[hi] *)
    if hi = mid then
      (Array.unsafe_get arr lo) = key
    else binarySearchAux arr lo mid key
  else (* a[lo] =< a[mid] < key <= a[hi] *)
    if lo = mid then
      (Array.unsafe_get arr hi) = key
    else binarySearchAux arr mid hi key

let binarySearch (key : element) (sorted : element array) : bool =
  let len = Array.length sorted in
  if len = 0 then false
  else
    let lo = Array.unsafe_get sorted 0 in
    if key < lo then false
    else
      let hi = Array.unsafe_get sorted (len - 1) in
      if key > hi then false
      else binarySearchAux sorted 0 (len - 1) key

|}

let make_predicate tag ss =
  let array_ident = "sorted_" ^ tag in
  let array =
    SSet.fold
      (fun s acc -> acc ^ "\"" ^ s ^ "\";\n  ")
      ss
      ("let " ^ array_ident ^ " = [|\n  ")
    ^ "|]"
  in
  let fn_ident = "is_" ^ tag in
  let fn = "let " ^ fn_ident ^ " s = binarySearch s " ^ array_ident in
  array ^ "\n\n" ^ fn ^ "\n\n"

let main words_file output_file =
  let predefined_words = get_predefined_words words_file in
  let predefined_words = SSet.union predefined_words reserved_words in
  let oc = open_out_bin output_file in
  output_string oc license;
  output_string oc binary_search;
  output_string oc (make_predicate "js_keyword" js_keywords);
  output_string oc (make_predicate "js_special_word" js_special_words);
  output_string oc (make_predicate "reserved" predefined_words);
  close_out oc

let () = main Sys.argv.(1) Sys.argv.(2)
