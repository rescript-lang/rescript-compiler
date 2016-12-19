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

type  file_group = 
  { dir : string ;
    sources : Binary_cache.t ; 
    resources : string list ;
    bs_dependencies : string list ;
    public : public
  } 

let (//) = Ext_filename.combine

let (|?)  m (key, cb) =
  m  |> Bsb_json.test key cb 

let get_list_string  =  Bsb_build_util.get_list_string

module String_vect = Resize_array.Make(struct type t = string let null = "" end)

let print_arrays file_array oc offset  =
  let indent = String.make offset ' ' in 
  let p_str s = 
    output_string oc indent ; 
    output_string oc s ;
    output_string oc "\n"
  in
  let len = String_vect.length file_array in 
  match len with 
  | 0
    -> output_string oc "[ ]\n"
  | 1 
    -> output_string oc ("[ \"" ^ String_vect.get file_array 0  ^ "\" ]\n")
  | _ (* first::(_::_ as rest) *)
    -> 
    output_string oc "[ \n";
    String_vect.iter_range ~from:0 ~to_:(len - 2 ) 
      (fun s -> p_str @@ "\"" ^ s ^ "\",") file_array;
    p_str @@ "\"" ^ (String_vect.last file_array) ^ "\"";

    p_str "]" 




let  handle_list_files dir (s : Bsb_json.t array) loc_start loc_end : Ext_file_pp.interval list * Binary_cache.t =  
  if Array.length s  = 0 then 
    begin 
      let files_array = Bsb_dir.readdir dir  in 
      let dyn_file_array = String_vect.make (Array.length files_array) in 
      let files  =
        Array.fold_left (fun acc name -> 
            let new_acc = Binary_cache.map_update ~dir acc name in 
            if new_acc != acc then (* reference in-equality *)
              String_vect.push name  dyn_file_array ;
            new_acc

          ) String_map.empty files_array in 
      [{Ext_file_pp.loc_start ;
        loc_end; action = (`print (print_arrays dyn_file_array))}],
      files
    end

  else 
    [],
    Array.fold_left (fun acc (s : Bsb_json.t) ->
        match s with 
        | `Str {str = s} -> 
          Binary_cache.map_update ~dir acc s
        | _ -> acc
      ) String_map.empty s

(* we need add a new line in the end,
   otherwise it will be idented twice
*)
type t = 
  { files :  file_group list ; 
    intervals :  Ext_file_pp.interval list ;
    globbed_dirs : string list ; 
  }

let (++) 
    ({files = a; 
      intervals = b; 
      globbed_dirs;
     } : t)
    ({files = c; intervals = d; globbed_dirs = dirs2; 
     })
  : t 
  = 
  {files = a@c; 
   intervals =  b@d ;
   globbed_dirs = globbed_dirs @ dirs2;
  }

let empty = { files = []; intervals  = []; globbed_dirs = [];  }

let rec parsing_source cwd (x : Bsb_json.t String_map.t )
  : t =
  let dir = ref cwd in
  let sources = ref String_map.empty in
  let resources = ref [] in 
  let bs_dependencies = ref [] in
  let public = ref Export_none in 

  let update_queue = ref [] in 
  let globbed_dirs = ref [] in 

  let children = ref [] in 
  let children_update_queue = ref [] in 
  let children_globbed_dirs = ref [] in 
  let () = 
    x 
    |?  (Bsb_build_schemas.dir, `Str (fun s -> dir := cwd // Bsb_build_util.convert_path s))
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
            sources := 
              Array.fold_left (fun acc name -> 
                  if Filename.check_suffix name ".ml"  
                  || Filename.check_suffix name ".mli"
                  || Filename.check_suffix name ".mll"
                  || Filename.check_suffix name ".re"
                  || Filename.check_suffix name ".rei"
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
        if s = "all" then public := Export_all else 
        if s = "none" then public := Export_none else 
          failwith ("invalid str for" ^ s )
      ))
    |? (Bsb_build_schemas.public, `Arr (fun s -> 
        public := Export_set (String_set.of_list (get_list_string s ) )
      ) )
    |? (Bsb_build_schemas.subdirs, `Arr (fun s -> 
        let res  = 
          Array.fold_left (fun  origin json ->
              match json with 
              | `Obj m -> 
                parsing_source !dir  m  ++ origin
              | _ -> origin ) empty s in 
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
       public = !public
      } 
      :: !children;
    intervals = !update_queue @ !children_update_queue ;
    globbed_dirs = !globbed_dirs @ !children_globbed_dirs;
  } 


let  parsing_sources cwd (file_groups : Bsb_json.t array)  = 
  Array.fold_left (fun  origin x ->
      match x with 
      | `Obj map ->  
        parsing_source cwd map ++ origin
      | _ -> origin
    ) empty  file_groups 

