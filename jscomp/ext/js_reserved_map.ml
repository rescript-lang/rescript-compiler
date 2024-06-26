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


type element = string 

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

(** Words that can never be identifier's name.

    See https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Lexical_grammar#reserved_words
 *)
let sorted_js_keywords = [|
  "await";
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
  "enum";
  "export";
  "extends";
  "false";
  "finally";
  "for";
  "function";
  "if";
  "implements";
  "import";
  "in";
  "instanceof";
  "interface";
  "let";
  "new";
  "null";
  "package";
  "private";
  "protected";
  "public";
  "return";
  "static";
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
  "yield";
|]

let is_js_keyword s = binarySearch s sorted_js_keywords

(** Identifiers with special meanings.

    They can have different meanings depending on the context when used as identifier names, so it should be done carefully.

    See https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Lexical_grammar#identifiers_with_special_meanings

    However, these names are actually used with no problems today. Preventing this can be annoying.
 *)
let sorted_js_special_words = [|
  "arguments";
  "as";
  "async";
  "eval";
  "from";
  "get";
  "of";
  "set";
|]

let is_js_special_word s = binarySearch s sorted_js_special_words

(** Identifier names _might_ need to care about *)
let sorted_js_globals = [|
  "AggregateError";
  "Array";
  "ArrayBuffer";
  "AsyncFunction";
  "AsyncGenerator";
  "AsyncGeneratorFunction";
  "AsyncIterator";
  "Atomics";
  "BigInt";
  "BigInt64Array";
  "BigUint64Array";
  "Boolean";
  "Bun";
  "DataView";
  "Date";
  "Deno";
  "Error";
  "EvalError";
  "FinalizationRegistry";
  "Float16Array";
  "Float32Array";
  "Float64Array";
  "Function";
  "Generator";
  "GeneratorFunction";
  "Infinity";
  "Int16Array";
  "Int32Array";
  "Int8Array";
  "Intl";
  "Iterator";
  "JSON";
  "Map";
  "Math";
  "NaN";
  "Number";
  "Object";
  "Promise";
  "Proxy";
  "RangeError";
  "ReferenceError";
  "Reflect";
  "RegExp";
  "Set";
  "SharedArrayBuffer";
  "String";
  "Symbol";
  "SyntaxError";
  "TypeError";
  "TypedArray";
  "URIError";
  "Uint16Array";
  "Uint32Array";
  "Uint8Array";
  "Uint8ClampedArray";
  "WeakMap";
  "WeakRef";
  "WeakSet";
  "__dirname";
  "__filename";
  "decodeURI";
  "decodeURIComponent";
  "encodeURI";
  "encodeURIComponent";
  "eval";
  "exports";
  "globalThis";
  "isFinite";
  "isNaN";
  "module";
  "parseFloat";
  "parseInt";
  "require";
  "undefined";
|]

let is_js_global s = binarySearch s sorted_js_globals
