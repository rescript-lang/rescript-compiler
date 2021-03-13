(* Copyright (C) 2019- Hongbo Zhang, Authors of ReScript
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




  
let usage = "Usage: [prog] [extra_args] <infile> <outfile>\n%!"
let main impl intf =
  try
    let a = Sys.argv in
    let n = Array.length a in
    if n > 2 then begin
      Arg.parse_argv (Array.sub Sys.argv 0 (n-2))
        [
          ("-bs-jsx",
           Arg.Int (fun i -> Js_config.jsx_version := i),
           " Set jsx version"
          )
        ] ignore usage;
      Ppx_apply.apply_lazy ~source:a.(n - 2) ~target:a.(n - 1)
        impl
        intf
    end else
      begin
        Printf.eprintf "%s" usage;
        exit 2
      end
  with exn ->
    begin
      Location.report_exception Format.err_formatter exn;
      exit 2
    end
