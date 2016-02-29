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


type env = 
  | Browser
  | NodeJS


let default_env = ref NodeJS 

let get_env () = !default_env

let set_env env = default_env := env 

let stdlib_set = String_set.of_list [
    "arg.js";
    "gc.js";
    "printexc.js";
    "array.js";
    "genlex.js";
    "printf.js";
    "arrayLabels.js";
    "hashtbl.js";
    "queue.js";
    "buffer.js";			
    "int32.js";
    "random.js";
    "bytes.js";			
    "int64.js";
    "scanf.js";
    "bytesLabels.js";
    "lazy.js";
    "set.js";
    "callback.js";
    "lexing.js";
    "sort.js";
    "camlinternalFormat.js";
    "list.js";
    "stack.js";
    "camlinternalFormatBasics.js";
    "listLabels.js";
    "stdLabels.js";
    "camlinternalLazy.js";
    "map.js";
    "std_exit.js";
    "camlinternalMod.js";
    "marshal.js";
    "stream.js";
    "camlinternalOO.js";
    "moreLabels.js";
    "string.js";
    "char.js";
    "nativeint.js";
    "stringLabels.js";
    "complex.js";
    "obj.js";
    "sys.js";
    "digest.js";
    "oo.js";
    "weak.js";
    "filename.js";
    "parsing.js";
    "format.js";
    "pervasives.js"
]

let runtime_set = String_set.of_list [
    "caml_array.js";
    "caml_float.js";
    "caml_obj.js";
    "caml_bigarray.js";
    "caml_format.js";		
    "caml_oo.js";		
    "caml_c_ffi.js";
    "caml_int64.js";
    "caml_primitive.js";
    "caml_utils.js";
    "caml_builtin_exceptions.js";
    "caml_exceptions.js";
    "caml_curry.js";
    "caml_file.js";
    "caml_lexer.js";
    "caml_parser.js";
    "caml_string.js"
    (* "caml_sys.js"; *)
    (* "caml_unix.js"; *)
    (* "caml_io.js"; *)
]


let prim = "Caml_primitive" 

let builtin_exceptions = "Caml_builtin_exceptions"


let io = "Caml_io"

let sys = "Caml_sys"

let lexer = "Caml_lexer"
let parser = "Caml_parser"
let obj_runtime = "Caml_obj"

let array = "Caml_array"

let format = "Caml_format"

let string = "Caml_string"

let float = "Caml_float"

let oo = "Caml_oo"
let curry = "Caml_curry"
let internalMod = "Caml_internalMod"
let bigarray = "Caml_bigarray"
let unix = "Caml_unix"
