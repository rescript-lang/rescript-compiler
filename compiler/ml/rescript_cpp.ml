(* Copyright (C) 2021- Hongbo Zhang, Authors of ReScript
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

type pp_error = Unterminated_if | Unterminated_else

exception Pp_error of pp_error * Location.t

type directive_value =
  | Dir_bool of bool
  | Dir_float of float
  | Dir_int of int
  | Dir_string of string
  | Dir_null

let prepare_pp_error loc = function
  | Unterminated_if -> Location.errorf ~loc "#if not terminated"
  | Unterminated_else -> Location.errorf ~loc "#else not terminated"

let () =
  Location.register_error_of_exn (function
    | Pp_error (err, loc) -> Some (prepare_pp_error loc err)
    | _ -> None)

let directive_built_in_values = Hashtbl.create 51

let replace_directive_built_in_value k v =
  Hashtbl.replace directive_built_in_values k v

let remove_directive_built_in_value k =
  Hashtbl.replace directive_built_in_values k Dir_null

let replace_directive_bool k v =
  Hashtbl.replace directive_built_in_values k (Dir_bool v)

let replace_directive_string k v =
  Hashtbl.replace directive_built_in_values k (Dir_string v)

let () =
  (* Note we use {!Config} instead of {!Sys} becasue
     we want to overwrite in some cases with the
     same stdlib
  *)
  let version = Config.version (* so that it can be overridden*) in
  replace_directive_built_in_value "OCAML_VERSION" (Dir_string version);
  replace_directive_built_in_value "OS_TYPE" (Dir_string Sys.os_type)

let iter_directive_built_in_value f = Hashtbl.iter f directive_built_in_values
(* let iter_directive_built_in_value f = Hashtbl.iter f directive_built_in_values *)

(*
     {[
       # semver 0 "12";;
       - : int * int * int * string = (12, 0, 0, "");;
       # semver 0 "12.3";;
       - : int * int * int * string = (12, 3, 0, "");;
         semver 0 "12.3.10";;
       - : int * int * int * string = (12, 3, 10, "");;
       # semver 0 "12.3.10+x";;
       - : int * int * int * string = (12, 3, 10, "+x")
     ]}
  *)

(** 
     {[
       semver Location.none "1.2.3" "~1.3.0" = false;;
       semver Location.none "1.2.3" "^1.3.0" = true ;;
       semver Location.none "1.2.3" ">1.3.0" = false ;;
       semver Location.none "1.2.3" ">=1.3.0" = false ;;
       semver Location.none "1.2.3" "<1.3.0" = true ;;
       semver Location.none "1.2.3" "<=1.3.0" = true ;;
     ]}
  *)

let pp_directive_value fmt (x : directive_value) =
  match x with
  | Dir_bool b -> Format.pp_print_bool fmt b
  | Dir_int b -> Format.pp_print_int fmt b
  | Dir_float b -> Format.pp_print_float fmt b
  | Dir_string s -> Format.fprintf fmt "%S" s
  | Dir_null -> Format.pp_print_string fmt "null"

let list_variables fmt =
  iter_directive_built_in_value (fun s dir_value ->
      Format.fprintf fmt "@[%s@ %a@]@." s pp_directive_value dir_value)

let define_key_value key v =
  if String.length key > 0 && Char.uppercase_ascii key.[0] = key.[0] then (
    replace_directive_built_in_value key
      (* NEED Sync up across {!lexer.mll} {!bspp.ml} and here,
         TODO: put it in {!lexer.mll}
      *)
      (try Dir_bool (bool_of_string v)
       with _ -> (
         try Dir_int (int_of_string v)
         with _ -> (
           try Dir_float (float_of_string v) with _ -> Dir_string v)));
    true)
  else false

type dir_conditional = Dir_if_true | Dir_out

(* let string_of_dir_conditional (x : dir_conditional) = *)
(*   match x with  *)
(*   | Dir_if_true -> "Dir_if_true" *)
(*   | Dir_if_false -> "Dir_if_false" *)
(*   | Dir_out -> "Dir_out" *)

let if_then_else = ref Dir_out

(* store the token after hash, [# token]
   when we see `#if` we do the processing immediately
   when we see #method, we produce `HASH` token and save `method`
   token so that the next lexing produce the right one.
*)
let sharp_look_ahead = ref None

let update_if_then_else v =
  (* Format.fprintf Format.err_formatter "@[update %s \n@]@." (string_of_dir_conditional v); *)
  if_then_else := v

let at_bol lexbuf =
  let pos = Lexing.lexeme_start_p lexbuf in
  pos.pos_cnum = pos.pos_bol

let eof_check lexbuf =
  if !if_then_else <> Dir_out then
    if !if_then_else = Dir_if_true then
      raise (Pp_error (Unterminated_if, Location.curr lexbuf))
    else raise (Pp_error (Unterminated_else, Location.curr lexbuf))

let init () =
  sharp_look_ahead := None;
  update_if_then_else Dir_out
