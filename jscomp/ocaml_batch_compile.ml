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


let batch_compile ppf files =
  if files <> [] then 
    begin 
      Compenv.readenv ppf Before_compile; 
      Compmisc.init_path  false;
      let batch_files  : (string, Ast_extract.ast) Hashtbl.t =
        Hashtbl.create 31 in 
      files |> List.iter begin fun name -> 
        match Ocaml_parse.check_suffix name with 
        | `Ml, opref -> 
          Hashtbl.add batch_files 
            name
            (Ml (Ocaml_parse.parse_implementation ppf name, opref) )
        | `Mli, opref -> 
          Hashtbl.add batch_files name
            (Mli (Ocaml_parse.parse_interface ppf name, opref))

      end;
      let stack, mapping = Ast_extract.prepare batch_files in 
      stack |> Queue.iter (fun modname -> 
          match Hashtbl.find_all mapping modname with
          | [] -> ()
          | [sourcefile] -> 
            begin match Hashtbl.find batch_files sourcefile with
              | exception _ -> assert false 
              | Ml (ast,opref) 
                ->
                Js_implementation.after_parsing_impl ppf sourcefile 
                  opref ast 
              | Mli (ast,opref)  
                ->
                Js_implementation.after_parsing_sig ppf sourcefile 
                  opref ast 
            end
          | [sourcefile1;sourcefile2] 
            -> (* TODO: check duplicated names *)
            begin match Hashtbl.find batch_files sourcefile1 with 
              | exception _ -> assert false 
              | Mli (ast,opref) -> 
                Js_implementation.after_parsing_sig ppf sourcefile1 opref ast ;
                begin match Hashtbl.find batch_files sourcefile2 with 
                  | Ml (ast,opref) -> 
                    Js_implementation.after_parsing_impl ppf sourcefile2 
                      opref ast ;
                  | _ -> assert false 
                end
              | Ml (ast0,opref0) -> 
                begin match Hashtbl.find batch_files sourcefile2 with 
                  | Mli (ast,opref) -> 
                    Js_implementation.after_parsing_sig ppf sourcefile2 opref ast ;
                    Js_implementation.after_parsing_impl ppf sourcefile1 
                      opref0 ast0 ;

                  | _ -> assert false
                end
            end
          | _ -> assert false 
        )
    end
