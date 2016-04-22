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
  | AmdJS
  | Goog of string option

let default_env = ref NodeJS 

let ext = ref ".js"
let get_ext () = !ext 
let get_env () = !default_env

let set_env env = default_env := env 
let cmd_set_module str = 
  match str with 
  | "commonjs" -> default_env := NodeJS
  | "amdjs" -> 
    default_env := AmdJS
  | "browser-internal" -> (* used internal *)
    default_env := Browser
  | _ -> 
    if Ext_string.starts_with str "goog" then
      let len = String.length str in
      if  len = 4  then
            begin 
              default_env := Goog (Some "");
              ext := ".g.js"
            end
      else
      if str.[4] = ':' && len > 5 then 
        begin 
          default_env := Goog (Some (Ext_string.tail_from str 5  ));
          ext := ".g.js";
        end
      else 
        raise (Arg.Bad (Printf.sprintf "invalid module system %s" str))
    else
      raise (Arg.Bad (Printf.sprintf "invalid module system %s" str))

let get_goog_package_name () = 
  match !default_env with 
  | Goog x -> x 
  | Browser
  | AmdJS
  | NodeJS -> None

let default_gen_tds = ref false
     
let stdlib_set = String_set.of_list [
    "arg";
    "gc";
    "printexc";
    "array";
    "genlex";
    "printf";
    "arrayLabels";
    "hashtbl";
    "queue";
    "buffer";			
    "int32";
    "random";
    "bytes";			
    "int64";
    "scanf";
    "bytesLabels";
    "lazy";
    "set";
    "callback";
    "lexing";
    "sort";
    "camlinternalFormat";
    "list";
    "stack";
    "camlinternalFormatBasics";
    "listLabels";
    "stdLabels";
    "camlinternalLazy";
    "map";

    (* "std_exit"; *)
    (* https://developer.mozilla.org/de/docs/Web/Events/beforeunload *)
    "camlinternalMod";
    "marshal";
    "stream";
    "camlinternalOO";
    "moreLabels";
    "string";
    "char";
    "nativeint";
    "stringLabels";
    "complex";
    "obj";
    "sys";
    "digest";
    "oo";
    "weak";
    "filename";
    "parsing";
    "format";
    "pervasives"
]


let prim = "Caml_primitive" 
let builtin_exceptions = "Caml_builtin_exceptions"
let exceptions = "Caml_exceptions"
let io = "Caml_io"
let sys = "Caml_sys"
let lexer = "Caml_lexer"
let parser = "Caml_parser"
let obj_runtime = "Caml_obj"
let array = "Caml_array"
let format = "Caml_format"
let string = "Caml_string"
let float = "Caml_float"
let hash = "Caml_hash"
let oo = "Caml_oo"
let curry = "Curry"
let bigarray = "Caml_bigarray"
let unix = "Caml_unix"
let int64 = "Caml_int64"
let md5 = "Caml_md5"
let weak = "Caml_weak"
let backtrace = "Caml_backtrace"
let gc = "Caml_gc"
let int32 = "Caml_int32"
let block = "Block"

let runtime_set = 
  [
    block;
    int32;
    gc ;
    backtrace; 
    prim;               
    builtin_exceptions ;
    exceptions ; 
    io ;
    sys ;
    lexer ;
    parser ;
    obj_runtime ;
    array ;
    format ;
    string ;
    float ;
    hash ;
    oo ;
    curry ;
    bigarray ;
    unix ;
    int64 ;
    md5 ;
    weak ] |> 
  List.fold_left (fun acc x -> String_set.add (String.uncapitalize x) acc ) String_set.empty
