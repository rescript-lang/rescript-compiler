
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




let replace s env : string = 
  Bsb_regex.global_substitute "\\${bsb:\\([-a-zA-Z0-9]+\\)}" 
    (fun (_s : string) templates -> 
       match templates with 
       | key::_ -> 
         String_hashtbl.find_exn  env key
       | _ -> assert false 
    ) s

let (//) = Filename.concat 
let npm_link = "npm link bs-platform"

(*let no_such_directory dir = 
    match Sys.is_directory dir with 
    | true -> false 
    | false -> true 
    |*)

let enter_dir cwd x action = 
  Unix.chdir x ; 
  match action () with 
  | exception e -> Unix.chdir cwd ; raise e 
  | v -> v 


let rec process_theme_aux env cwd (x : OCamlRes.Res.node) = 
  match x with 
  | File (name,content)  -> 
    Ext_io.write_file (cwd // name) (replace content env)
  | Dir (current, nodes) -> 
    Unix.mkdir (cwd//current) 0o777;
    List.iter (fun x -> process_theme_aux env (cwd//current) x ) nodes

let list_themes () =
  Format.fprintf Format.std_formatter "Available themes: @.";
  Bsb_templates.root 
  |>
  List.iter (fun (x : OCamlRes.Res.node)  ->
      match  x with 
      | Dir (x, _) -> 
        Format.fprintf Format.std_formatter "%s@." x 

      | _ -> ()
    ) 

(* @raise [Not_found] *)  
let process_themes env theme proj_dir (themes : OCamlRes.Res.node list ) = 
  match List.find (fun (x : OCamlRes.Res.node) -> 
      match  x with 
      | Dir (dir, _) -> dir = theme
      | File _ -> false 
    ) themes  with 
  | exception Not_found -> 
    list_themes ();
    raise (Arg.Bad( "theme " ^ theme ^ " not found")  )
  | Dir(_theme, nodes ) -> 
    List.iter (fun node -> process_theme_aux env proj_dir node ) nodes
  | _ -> assert false  

(** TODO: run npm link *)
let init_sample_project ~cwd ~theme name = 
  let env = String_hashtbl.create 0 in 
  List.iter (fun (k,v) -> String_hashtbl.add env k v  ) [  
    "proj-version", "0.1.0";
    "bs-version", Bs_version.version;
    "bsb" , Filename.current_dir_name // "node_modules" // ".bin" // "bsb"
  ];
  let action = fun _ -> 
    process_themes env  theme Filename.current_dir_name Bsb_templates.root;         
    Format.fprintf Format.std_formatter "Running %s in %s @." npm_link (cwd//name);
    let exit_code = Sys.command npm_link in 
    if exit_code <> 0 then 
      begin
        prerr_endline ("failed to run : " ^ npm_link);
        exit exit_code
      end
  in   
  begin match name with 
    | "." -> 
      let name = Filename.basename cwd in
      if Ext_string.is_valid_npm_package_name name then 
        begin 
          String_hashtbl.add env "name" name;
          action ()
        end
      else 
        begin
          Format.fprintf Format.err_formatter 
            "@{<error>Invalid package name@} %S @."
            name ;
          exit 2    
        end

    | _ -> 
      if Ext_string.is_valid_npm_package_name name 
      then begin 
        Format.fprintf Format.std_formatter "Making directory %s@." name;  
        if Sys.file_exists name then 
          begin 
            Format.fprintf Format.err_formatter "%s already existed@." name ;
            exit 2
          end 
        else
          begin              
            Unix.mkdir name 0o777;     
            String_hashtbl.add env "name" name;
            enter_dir cwd name action
          end
      end else begin 
        Format.fprintf Format.err_formatter 
          "@{<error>Invalid package name@} %S @."
          name ;
        exit 2                        
      end 
  end




