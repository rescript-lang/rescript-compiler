
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

module String_set = Ast_extract.String_set





let read_ast (type t ) (kind : t  Ast_extract.kind) fn : t  =
  let magic =
    match kind with 
    | Ast_extract.Ml -> Config.ast_impl_magic_number
    | Ast_extract.Mli -> Config.ast_intf_magic_number in 
  let ic = open_in_bin fn in
  try
    let dep_size = input_binary_int ic in 
    seek_in  ic (pos_in ic + dep_size) ; 
    let buffer = really_input_string ic (String.length magic) in
    assert(buffer = magic); (* already checked by apply_rewriter *)
    Location.input_name := input_value ic;
    let ast = input_value ic in
    close_in ic;
    ast
  with exn ->
    close_in ic;
    raise exn


(*
   Reasons that we don't [output_value] the set:
   1. for performance , easy skipping and calcuate the length 
   2. cut dependency, otherwise its type is {!Ast_extract.String_set.t}
*)      
let write_ast (type t) ~(fname : string) ~output (kind : t Ast_extract.kind) ( pt : t) : unit =
  let magic = 
    match kind with 
    | Ast_extract.Ml -> Config.ast_impl_magic_number
    | Ast_extract.Mli -> Config.ast_intf_magic_number in
  let oc = open_out_bin output in 

  let output_set = Ast_extract.read_parse_and_extract kind pt in
  let buf = Buffer.create 64 in
  let number = String_set.cardinal output_set in 
  Buffer.add_string buf (string_of_int number) ;
  Buffer.add_char buf '\t';
  String_set.iter (fun s -> Buffer.add_string buf s ; Buffer.add_char buf '\t') output_set ;
  let buf_contents = Buffer.contents buf in 
  output_binary_int oc (String.length buf_contents);
  output_string oc buf_contents; 

  output_string oc magic ;
  output_value oc fname;
  output_value oc pt;
  close_out oc 

let write_ast_simple (type t) ~(fname : string) ~output (kind : t Ast_extract.kind) ( pt : t) : unit =
  let magic =
    match kind with
    | Ast_extract.Ml -> Config.ast_impl_magic_number
    | Ast_extract.Mli -> Config.ast_intf_magic_number in
  let oc = open_out_bin output in
  output_string oc magic;
  output_value oc fname;
  output_value oc pt;
  close_out oc;
