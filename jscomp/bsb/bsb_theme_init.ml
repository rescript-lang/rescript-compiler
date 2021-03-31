
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
  Bsb_regex.global_substitute s ~reg:"\\${rescript:\\([-a-zA-Z0-9]+\\)}"
    (fun (_s : string) templates ->
       match templates with
       | key::_ ->
         Hash_string.find_exn  env key
       | _ -> assert false
    ) 

let (//) = Filename.concat



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

let rec process_theme_aux env cwd (x : Bsb_templates.node) =
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
  Ext_list.iter Bsb_templates.root (fun x  ->
      match  x with
      | Dir (x, _) ->
        Format.fprintf Format.std_formatter "%s@." x

      | _ -> ()
    )

(* @raise [Not_found] *)
let process_themes env theme proj_dir (themes : Bsb_templates.node list ) =
  match Ext_list.find_first themes (fun x ->
      match  x with
      | Dir (dir, _) -> dir = theme
      | File _ -> false
    )  with
  | None ->
    list_themes ();
    Bsb_arg.bad_arg ( "theme " ^ theme ^ " not found")
  | Some (Dir(_theme, nodes )) ->
    List.iter (fun node -> process_theme_aux env proj_dir node ) nodes
  | Some _ -> assert false

(** TODO: run npm link *)
let init_sample_project ~cwd ~theme name =
  let env = Hash_string.create 0 in
  List.iter (fun (k,v) -> Hash_string.add env k v  ) [
    "proj-version", "0.1.0";
    "bs-version", Bs_version.version;
    "bsb" , Filename.current_dir_name // "node_modules" // ".bin" // "bsb";
    "platform", !Bs_version.package_name
  ];
  let action = fun _ ->
    process_themes env  theme Filename.current_dir_name Bsb_templates.root
  in
  begin match name with
    | "." ->
      let name = Filename.basename cwd in
      if Ext_namespace.is_valid_npm_package_name name then
        begin
          Hash_string.add env "name" name;
          action ()
        end
      else
        begin
          Format.fprintf Format.err_formatter
            "@{<error>Invalid package name@} %S@}: the project name must be both a valid npm package name and a valid name as namespace@."
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
            Hash_string.add env "name" name;
            enter_dir cwd name action
          end
        | Non_exists
          ->
          begin
            Format.fprintf Format.std_formatter "Making directory %s@." name;
            Unix.mkdir name 0o777;            
            Hash_string.add env "name" name;
            enter_dir cwd name action
          end
      end else begin
        Format.fprintf Format.err_formatter
          "@{<error>Invalid package name@} %S.@} The project name must be a valid npm name, thus can't contain upper-case letters, for example."
          name ;
        exit 2
      end
  end




