
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


type file_type = 
  | Directory
  | Non_directory_file
  | Non_exists 

let classify_file name = 
  let exists = Sys.file_exists name in 
  if exists then 
    if Sys.is_directory name then Directory
    else Non_directory_file
  else Non_exists   

let replace s env : string =
  Bsb_regex.global_substitute "\\${bsb:\\([-a-zA-Z0-9]+\\)}"
    (fun (_s : string) templates ->
       match templates with
       | key::_ ->
         String_hashtbl.find_exn  env key
       | _ -> assert false
    ) s

let (//) = Filename.concat

(* TODO: Check Ext_io.write_file may overwrite, duplicate with Bsb_config_parse *)
let get_bs_platform_version_if_exists dir = 
  match 
    Ext_json_parse.parse_json_from_file 
    (Filename.concat dir Literals.package_json) with 
  | Obj {map} 
    -> 
    (match String_map.find_exn map Bsb_build_schemas.version with 
    | Str {str} -> str 
    | _ -> assert false)
  | _ -> assert false 

let run_npm_link cwd dirname  =
  let bs_platform_dir =  
    Filename.concat Literals.node_modules Bs_version.package_name in 
  if Sys.file_exists bs_platform_dir
  then  
    if get_bs_platform_version_if_exists bs_platform_dir = Bs_version.version then 
      begin 
        Format.fprintf Format.std_formatter 
          "bs-platform already exists(version match), no need symlink@."
      end 
    else   
      begin 
        Format.fprintf Format.err_formatter 
        "bs-platform already exists, but version mismatch with running bsb@.";
        exit 2
      end 
  else 
  if Ext_sys.is_windows_or_cygwin then
    begin
      let npm_link = "npm link bs-platform" in
      let exit_code = Sys.command npm_link in
      if exit_code <> 0 then
        begin
          prerr_endline ("failed to run : " ^ npm_link);
          exit exit_code
        end
    end
  else
    begin
      (* symlink bs-platform and bsb,bsc,bsrefmt to .bin directory
        we did not run npm link bs-platform for efficiency reasons
      *)
      Format.fprintf Format.std_formatter "Symlink bs-platform in %s @."  (cwd//dirname);
      let (//) = Filename.concat in
      let node_bin =  "node_modules" // ".bin" in
      Bsb_build_util.mkp node_bin;
      let p = ".." // Bs_version.package_name // "lib" in
      let link a =
        Unix.symlink (p//a) (node_bin // a) in
      link "bsb" ;
      link "bsc" ;
      link "bsrefmt";
      Unix.symlink
        (Filename.dirname (Filename.dirname Sys.executable_name))
        (Filename.concat "node_modules" Bs_version.package_name)
    end

let enter_dir cwd x action =
  Unix.chdir x ;
  match action () with
  | exception e -> Unix.chdir cwd ; raise e
  | v -> v

let mkdir_or_not_if_exists dir = 
  match classify_file dir with 
  | Directory -> ()
  | Non_directory_file 
    -> 
    Format.fprintf Format.err_formatter 
     "%s expected to be added as dir but exist file is not a dir" dir
  | Non_exists -> Unix.mkdir dir 0o777

let rec process_theme_aux env cwd (x : OCamlRes.Res.node) =
  match x with
  | File (name,content)  ->
    let new_file = cwd // name in 
    if not @@ Sys.file_exists new_file then
      Ext_io.write_file new_file (replace content env)
  | Dir (current, nodes) ->
    let new_cwd = cwd // current in 
    mkdir_or_not_if_exists new_cwd;
    List.iter (fun x -> process_theme_aux env new_cwd x ) nodes

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
    run_npm_link cwd name
  in
  begin match name with
    | "." ->
      let name = Filename.basename cwd in
      if Ext_namespace.is_valid_npm_package_name name then
        begin
          String_hashtbl.add env "name" name;
          action ()
        end
      else
        begin
          Format.fprintf Format.err_formatter
            "@{<error>Invalid package name@} %S.@} The project name must be a valid npm name, thus can't contain upper-case letters, for example."
            name ;
          exit 2
        end

    | _ ->
      if Ext_namespace.is_valid_npm_package_name name
      then begin        
        match classify_file name with 
        | Non_directory_file 
          -> 
          begin
            Format.fprintf Format.err_formatter "@{<error>%s already exists but it is not a directory@}@." name ;
            exit 2
          end
        | Directory -> 
          begin
            Format.fprintf Format.std_formatter "Adding files into existing dir %s@." name; 
            String_hashtbl.add env "name" name;
            enter_dir cwd name action
          end
        | Non_exists
          ->
          begin
            Format.fprintf Format.std_formatter "Making directory %s@." name;
            Unix.mkdir name 0o777;            
            String_hashtbl.add env "name" name;
            enter_dir cwd name action
          end
      end else begin
        Format.fprintf Format.err_formatter
          "@{<error>Invalid package name@} %S.@} The project name must be a valid npm name, thus can't contain upper-case letters, for example."
          name ;
        exit 2
      end
  end




