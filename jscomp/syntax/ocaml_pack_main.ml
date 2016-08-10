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


let _ = 
  let _loc = Location.none in
  let argv = Sys.argv in
  let files = 
    if Array.length argv = 2 && Filename.check_suffix  argv.(1) "mllib" then 
      Line_process.read_lines (Sys.getcwd ())argv.(1)
    else 
      Array.to_list
        (Array.sub Sys.argv 1 (Array.length Sys.argv - 1)) 
  in 
  let tasks = Ocaml_extract.process files in 
  let emit name = 
    output_string stdout "#1 \"";
    (*Note here we do this is mostly to avoid leaking user's 
      information, like private path, in the future, we can have 
      a flag
    *)
    output_string stdout (Filename.basename name) ;
    output_string stdout "\"\n" 
  in
  begin 
    let local_time = Unix.(localtime (gettimeofday ())) in 
    output_string stdout 
      (Printf.sprintf {|(** Bundled by ocamlpack %02d/%02d-%02d:%02d *)|}
         (local_time.tm_mon + 1) local_time.tm_mday 
         local_time.tm_hour local_time.tm_min 
                         );
    output_string stdout "\n";
    tasks |> List.iter (fun t ->
        match t with
        | `All (base, ml_content,ml_name, mli_content, mli_name) -> 
          let base = String.capitalize base in 
          output_string stdout "module ";
          output_string stdout base ; 
          output_string stdout " : sig \n";

          emit mli_name ;
          output_string stdout mli_content;

          output_string stdout "\nend = struct\n";
          emit ml_name ;
          output_string stdout ml_content;
          output_string stdout "\nend\n"

        | `Ml (base, ml_content, ml_name) -> 
          let base = String.capitalize base in 
          output_string stdout "module \n";
          output_string stdout base ; 
          output_string stdout "\n= struct\n";

          emit ml_name;
          output_string stdout ml_content;

          output_string stdout "\nend\n"

        | `Mli (base, mli_content, mli_name) -> 
          let base = String.capitalize base in 
          output_string stdout "module type \n";
          output_string stdout base ; 
          output_string stdout "\n= sig\n";

          emit mli_name;
          output_string stdout mli_content;

          output_string stdout "\nend\n"
       
      )
  end

(* local variables: *)
(* compile-command: "ocamlbuild -no-hygiene -cflags -annot -use-ocamlfind -pkg compiler-libs.common ocaml_pack_main.byte " *)
(* end: *)
