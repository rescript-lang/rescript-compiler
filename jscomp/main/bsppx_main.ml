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



let apply_lazy ~source ~target impl iface =
  let ic = open_in_bin source in
  let magic =
    really_input_string ic (String.length Config.ast_impl_magic_number)
  in
  if magic <> Config.ast_impl_magic_number
  && magic <> Config.ast_intf_magic_number then
    failwith "Bs_ast_mapper: OCaml version mismatch or malformed input";
  Location.input_name := input_value ic;
  let ast = input_value ic in
  close_in ic;

  let ast =
    if magic = Config.ast_impl_magic_number
    then Obj.magic (impl (Obj.magic ast))
    else Obj.magic (iface (Obj.magic ast))
  in
  let oc = open_out_bin target in
  output_string oc magic;
  output_value oc !Location.input_name;
  output_value oc ast;
  close_out oc


let  () =
  try
    let a = Sys.argv in
    let n = Array.length a in
    if n > 2 then
      apply_lazy ~source:a.(n - 2) ~target:a.(n - 1)
        !Ppx_entry.rewrite_implementation
        !Ppx_entry.rewrite_signature
    else
      begin
        Printf.eprintf "Usage: %s [extra_args] <infile> <outfile>\n%!"
          Sys.executable_name;
        exit 2
      end
  with exn ->
    begin
      Location.report_exception Format.err_formatter exn;
      exit 2
    end

