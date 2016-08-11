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




let (@>) (b, v) acc = 
  if b then 
    v :: acc 
  else
      acc 


let (//) = Filename.concat

let rec process_line cwd filedir  line = 
  let line = Ext_string.trim line in 
  let len = String.length line in 
  if len = 0 then []
  else 
    match line.[0] with 
    | '#' -> []
    | _ -> 
      let segments = 
        Ext_string.split_by ~keep_empty:false (fun x -> x = ' ' || x = '\t' ) line 
      in
      begin 
        match segments with 
        | ["include" ;  path ]
          ->  
          (* prerr_endline path;  *)
          read_lines cwd  (filedir// path)
        | [ x ]  -> 
          let ml = filedir // x ^ ".ml" in 
          let mli = filedir // x ^ ".mli" in 
          let ml_exists, mli_exists = Sys.file_exists ml , Sys.file_exists mli in 
          if not ml_exists && not mli_exists then 
            begin 
              prerr_endline (filedir //x ^ " not exists"); 
              []
            end
          else 
            (ml_exists, ml) @> (mli_exists , mli) @> []            
        | _ 
          ->  Ext_pervasives.failwithf ~loc:__LOC__ "invalid line %s" line
      end
and read_lines (cwd : string) (file : string) : string list = 
  file 
  |> Ext_io.rev_lines_of_file 
  |> List.fold_left (fun acc f ->
      let filedir  =   Filename.dirname file in
      let extras = process_line  cwd filedir f in 
      extras  @ acc       
    ) []


let _ = 
  let _loc = Location.none in
  let argv = Sys.argv in
  let files = 
    if Array.length argv = 2 && Filename.check_suffix  argv.(1) "mllib" then 
      read_lines (Sys.getcwd ())argv.(1)
    else 
      Array.to_list
        (Array.sub Sys.argv 1 (Array.length Sys.argv - 1)) 
  in 
  let ast_table =
    Ast_extract.build
      Format.err_formatter files
      (fun _ppf sourcefile ->
         let content = Ext_io.load_file sourcefile in 
         Parse.implementation (Lexing.from_string content), content
      )
      (fun _ppf sourcefile ->
         let content = Ext_io.load_file sourcefile in
         Parse.interface (Lexing.from_string content), content         
      ) in
  let tasks = Ast_extract.sort fst  fst ast_table in
  
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
    tasks |> Queue.iter (fun module_ ->
      let t = (match (String_map.find  module_ ast_table).ast_info with
      | exception Not_found -> failwith (module_ ^ "not found")
      | Ml (sourcefile, (_, content), _)
        ->
        `Ml (module_, content, sourcefile)
      | Mli (sourcefile , (_, content), _) -> 
          `Mli (module_, content, sourcefile)
      | Ml_mli (ml_sourcefile, (_, ml_content), _, mli_sourcefile,  (_, mli_content), _)
        ->
        `All (module_, ml_content, ml_sourcefile, mli_content, mli_sourcefile)) in         
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
