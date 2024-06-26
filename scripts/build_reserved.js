// @ts-check

const fs = require("fs");
const path = require("path");

const jscompDir = path.join(__dirname, "..", "jscomp");
const reservedMap = path.join(jscompDir, "ext", "js_reserved_map.ml");

const licenseSnippet = `
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

`.trimStart();

const binarySearchSnippet = `
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
`.trimStart();

/**
 * @param {string} tag
 * @param {Set<string>} words
 * @return {string}
 */
function printPredicate(tag, words) {
  const sorted = [...words].sort();
  return `
let sorted_${tag}s = [|
${sorted.map(word => `  "${word}";`).join("\n")}
|]

let is_${tag} s = binarySearch s sorted_${tag}s
`.trimStart();
}

function print() {
  return `
${licenseSnippet}
${binarySearchSnippet}
(** Words that can never be identifier's name.

    See https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Lexical_grammar#reserved_words
 *)
${printPredicate(
  "js_keyword",
  new Set([
    "break",
    "case",
    "catch",
    "class",
    "const",
    "continue",
    "debugger",
    "default",
    "delete",
    "do",
    "else",
    "export",
    "extends",
    "false",
    "finally",
    "for",
    "function",
    "if",
    "import",
    "in",
    "instanceof",
    "new",
    "null",
    "return",
    "super",
    "switch",
    "this",
    "throw",
    "true",
    "try",
    "typeof",
    "var",
    "void",
    "while",
    "with",
    // The following are also reserved in strict context, including ESM
    "let",
    "static",
    "yield",
    // `await` is reserved in async context, including ESM
    "await",
    // Future reserved words
    "enum",
    "implements",
    "interface",
    "package",
    "private",
    "protected",
    "public",
  ])
)}
(** Identifiers with special meanings.

    They can have different meanings depending on the context when used as identifier names, so it should be done carefully.

    See https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Lexical_grammar#identifiers_with_special_meanings

    However, these names are actually used with no problems today. Preventing this can be annoying.
 *)
${printPredicate(
  "js_special_word",
  new Set(["arguments", "as", "async", "eval", "from", "get", "of", "set"])
)}
(** Identifier names _might_ need to care about *)
${printPredicate(
  "js_global",
  new Set([
    // JavaScript standards built-ins
    // See https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects
    "AggregateError",
    "Array",
    "ArrayBuffer",
    "AsyncFunction",
    "AsyncGenerator",
    "AsyncGeneratorFunction",
    "AsyncIterator",
    "Atomics",
    "BigInt",
    "BigInt64Array",
    "BigUint64Array",
    "Boolean",
    "DataView",
    "Date",
    "decodeURI",
    "decodeURIComponent",
    "encodeURI",
    "encodeURIComponent",
    "Error",
    "eval",
    "EvalError",
    "FinalizationRegistry",
    "Float16Array",
    "Float32Array",
    "Float64Array",
    "Function",
    "Generator",
    "GeneratorFunction",
    "globalThis",
    "Infinity",
    "Int16Array",
    "Int32Array",
    "Int8Array",
    "Intl",
    "isFinite",
    "isNaN",
    "Iterator",
    "JSON",
    "Map",
    "Math",
    "NaN",
    "Number",
    "Object",
    "parseFloat",
    "parseInt",
    "Promise",
    "Proxy",
    "RangeError",
    "ReferenceError",
    "Reflect",
    "RegExp",
    "Set",
    "SharedArrayBuffer",
    "String",
    "Symbol",
    "SyntaxError",
    "TypedArray",
    "TypeError",
    "Uint16Array",
    "Uint32Array",
    "Uint8Array",
    "Uint8ClampedArray",
    "undefined",
    "URIError",
    "WeakMap",
    "WeakRef",
    "WeakSet",

    // A few of the HTML standard globals
    //
    // See https://developer.mozilla.org/en-US/docs/Web/API/Window
    // See https://developer.mozilla.org/en-US/docs/Web/API/WorkerGlobalScope
    //
    // "window",
    // "self",
    // "document",
    // "location",
    // "navigator",
    // "origin",

    // A few of the Node.js globals
    //
    // See https://nodejs.org/api/globals.html
    //
    // This is only useful for CommonJS modules in Node.js, where redeclaration is not allowed.
    // It is a workaround, not a solution.
    "__dirname",
    "__filename",
    "Buffer",
    "setImmediate",
    "clearImmediate",
    "setTimeout",
    "clearTimeout",
    "setInterval",
    "clearInterval",
    "global",
    "console",
    "process",
    "require",
    "module",
    "exports",

    // Bun's global namespace
    "Bun",

    // Deno's global namespace
    "Deno",
  ])
)}`.trimStart();
}

fs.writeFileSync(reservedMap, print(), "utf8");
