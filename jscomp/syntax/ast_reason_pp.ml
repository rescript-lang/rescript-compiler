(* Copyright (C) 2019- Authors of BuckleScript
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



exception Pp_error
(* Sync up with {!Pparse.preprocess} 
  The generated file should not sit 
  in the same directory as sourctree
*)
let pp (sourcefile : string) =
  Location.input_name := sourcefile;
  let tmpfile = Filename.temp_file "ocamlpp" "" in
  let pp = (*TODO: check to avoid double quoting *)
    Ext_filename.maybe_quote 
      (match !Js_config.refmt with 
       | None ->
         Filename.concat (Filename.dirname Sys.executable_name) "refmt.exe" 
       | Some x -> x)
  in 
  let comm = Printf.sprintf "%s --print=binary %s > %s"
      pp (Ext_filename.maybe_quote sourcefile) tmpfile
  in
  if Sys.command comm <> 0 then begin
    (try Sys.remove tmpfile with _ -> ());
    raise Pp_error
  end;
  tmpfile  

