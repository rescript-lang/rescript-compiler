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

type public = 
  | Export_all 
  | Export_set of String_set.t 
  | Export_none

type dir_index = int 
let lib_dir_index = 0

let get_dev_index, get_current_number_of_dev_groups =
  let dir_index = ref 0 in 
  ((fun () -> incr dir_index ; !dir_index),
   (fun _ -> !dir_index ))



(** 
   0 : lib 
   1 : dev 1 
   2 : dev 2 
*)  
type  file_group = 
  { dir : string ;
    sources : Binary_cache.file_group_rouces; 
    resources : string list ;
    bs_dependencies : string list ;
    public : public ;
    dir_index : dir_index 
  } 

(**
    [intervals] are used for side effect so we can patch `bsconfig.json` to add new files 
     we need add a new line in the end,
     otherwise it will be idented twice
*)

type t = 
  { files :  file_group list ; 
    intervals :  Ext_file_pp.interval list ;    
    globbed_dirs : string list ; 
  }

let (//) = Ext_filename.combine

let (|?)  m (key, cb) =
  m  |> Ext_json.test key cb 

let get_list_string  =  Bsb_build_util.get_list_string



let print_arrays file_array oc offset  =
  let indent = String.make offset ' ' in 
  let p_str s = 
    output_string oc indent ; 
    output_string oc s ;
    output_string oc "\n"
  in
  let len = String_vec.length file_array in 
  match len with 
  | 0
    -> output_string oc "[ ]\n"
  | 1 
    -> output_string oc ("[ \"" ^ String_vec.get file_array 0  ^ "\" ]\n")
  | _ (* first::(_::_ as rest) *)
    -> 
    output_string oc "[ \n";
    String_vec.iter_range ~from:0 ~to_:(len - 2 ) 
      (fun s -> p_str @@ "\"" ^ s ^ "\",") file_array;
    p_str @@ "\"" ^ (String_vec.last file_array) ^ "\"";

    p_str "]" 




let  handle_list_files dir (s : Ext_json.t array) loc_start loc_end : Ext_file_pp.interval list * _ =  
  if  Ext_array.is_empty s  then 
    begin 
      let files_array = Bsb_dir.readdir dir  in 
      let dyn_file_array = String_vec.make (Array.length files_array) in 
      let files  =
        Array.fold_left (fun acc name -> 
            if Ext_string.is_valid_source_name name then begin 
              let new_acc = Binary_cache.map_update ~dir acc name  in 
              String_vec.push name dyn_file_array ;
              new_acc 
            end else acc 
          ) String_map.empty files_array in 
      [{Ext_file_pp.loc_start ;
        loc_end; action = (`print (print_arrays dyn_file_array))}],
      files
    end

  else 
    [],
    Array.fold_left (fun acc (s : Ext_json.t) ->
        match s with 
        | `Str {str = s} -> 
          Binary_cache.map_update ~dir acc s
        | _ -> acc
      ) String_map.empty s


let empty = { files = []; intervals  = []; globbed_dirs = [];  }



let (++) (u : t)  (v : t)  = 
  if u == empty then v 
  else if v == empty then u 
  else 
    {
      files = u.files @ v.files ; 
      intervals = u.intervals @ v.intervals ; 
      globbed_dirs = u.globbed_dirs @ v.globbed_dirs ; 
    }


(** [dir_index] can be inherited  *)
let rec 
  parsing_simple_dir dir_index  cwd dir =
  parsing_source dir_index cwd 
    (`Obj (String_map.singleton Bsb_build_schemas.dir dir))
and parsing_source (dir_index : int) cwd (x : Ext_json.t )
  : t  =
  match x with 
  | `Str _ as dir -> 
    parsing_simple_dir dir_index cwd dir   
  | `Obj x -> 
    let dir = ref cwd in
    let sources = ref String_map.empty in
    let resources = ref [] in 
    let bs_dependencies = ref [] in
    (* let public = ref Export_none in  *)
    (* let public = ref Bsb_config.default_public in *)
    let public = ref Export_all in (* TODO: move to {!Bsb_default} later*)

    let current_dir_index = ref dir_index in 
    (** Get the real [dir_index] *)
    let () = 
      x |?  (Bsb_build_schemas.type_, 
             `Str (fun s -> 
                 if Ext_string.equal s   Bsb_build_schemas.dev then
                   current_dir_index := get_dev_index ()
               )) |> ignore  in 
    let current_dir_index = !current_dir_index in 
    if !Bsb_config.no_dev && current_dir_index <> lib_dir_index then empty
    else 
      let update_queue = ref [] in 
      let globbed_dirs = ref [] in 

      let children = ref [] in 
      let children_update_queue = ref [] in 
      let children_globbed_dirs = ref [] in 
      let () = 
        x     
        |?  (Bsb_build_schemas.dir, `Str (fun s -> dir := cwd // Ext_filename.simple_convert_node_path_to_os_path s))
        |?  (Bsb_build_schemas.files ,
             `Arr_loc (fun s loc_start loc_end ->
                 let dir = !dir in 
                 let tasks, files =  handle_list_files  dir s loc_start loc_end in
                 update_queue := tasks ;
                 sources := files

               ))
        |? (Bsb_build_schemas.files,
            `Not_found (fun _ ->
                let dir = !dir in 
                let file_array = Bsb_dir.readdir dir in 
                (** We should avoid temporary files *)
                sources := 
                  Array.fold_left (fun acc name -> 
                      if Ext_string.is_valid_source_name name 
                      then 
                        Binary_cache.map_update  ~dir acc name 
                      else acc
                    ) String_map.empty file_array;
                globbed_dirs :=  [dir]
              )
           )     
        |? (Bsb_build_schemas.files, 
            `Obj (fun m -> 
                let excludes = ref [] in 
                m
                |? (Bsb_build_schemas.excludes,
                    `Arr (fun arr ->  excludes := get_list_string arr))
                |? (Bsb_build_schemas.slow_re, 
                    `Str 
                      (fun s -> 
                         let re = Str.regexp s in 
                         let dir = !dir in 
                         let excludes = !excludes in 
                         let file_array = Bsb_dir.readdir dir in 
                         sources := 
                           Array.fold_left (fun acc name -> 
                               if Str.string_match re name 0 && 
                                  not (List.mem name excludes)
                               then 
                                 Binary_cache.map_update  ~dir acc name 
                               else acc
                             ) String_map.empty file_array;
                         globbed_dirs :=  [dir]
                      ))
                |> ignore
              )
           )             
        |? (Bsb_build_schemas.bs_dependencies, `Arr (fun s -> bs_dependencies := get_list_string s ))
        |?  (Bsb_build_schemas.resources ,
             `Arr (fun s  ->
                 resources := get_list_string s
               ))
        |? (Bsb_build_schemas.public, `Str (fun s -> 
            if s = Bsb_build_schemas.export_all then public := Export_all else 
            if s = Bsb_build_schemas.export_none then public := Export_none else 
              failwith ("invalid str for" ^ s )
          ))
        |? (Bsb_build_schemas.public, `Arr (fun s -> 
            public := Export_set (String_set.of_list (get_list_string s ) )
          ) )
        |? (Bsb_build_schemas.subdirs, `Id (fun s -> 
            let res  = parsing_sources current_dir_index !dir s in 

            children :=  res.files ; 
            children_update_queue := res.intervals;
            children_globbed_dirs := res.globbed_dirs
          ))
        |> ignore 
      in 
      {
        files = 
          {dir = !dir; 
           sources = !sources; 
           resources = !resources;
           bs_dependencies = !bs_dependencies;
           public = !public;
           dir_index = current_dir_index;
          } 
          :: !children;
        intervals = !update_queue @ !children_update_queue ;
        globbed_dirs = !globbed_dirs @ !children_globbed_dirs;
      } 
  | _ -> empty 
(* and parsing_simple_dir dir_index cwd  dir  : t = 
   parsing_source dir_index cwd (String_map.singleton Bsb_build_schemas.dir dir)
*)

and  parsing_arr_sources dir_index cwd (file_groups : Ext_json.t array)  = 
  Array.fold_left (fun  origin x ->
      parsing_source dir_index cwd x ++ origin 
    ) empty  file_groups 

and  parsing_sources dir_index cwd (sources : Ext_json.t )  = 
  match sources with   
  | `Arr file_groups -> 
    parsing_arr_sources dir_index cwd file_groups.Ext_json.content
  | _ -> parsing_source dir_index cwd sources



  