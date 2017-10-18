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

let dir_cache = 
  String_hashtbl.create 32 

(** Only cached in the life-time of a single process *)  
let readdir path = 
  match String_hashtbl.find_opt dir_cache path with
  | None -> 
    let result = Sys.readdir path  in 
    String_hashtbl.add dir_cache path result ; 
    result 
  | Some result -> result 


type public = 
  | Export_all 
  | Export_set of String_set.t 
  | Export_none


type build_generator = 
  { input : string list ;
    output : string list;
    command : string}


let is_input_or_output(xs : build_generator list) (x : string)  = 
  List.exists 
    (fun  ({input; output} : build_generator) -> 
       let it_is = (fun y -> y = x ) in
       List.exists it_is input ||
       List.exists it_is output
    ) xs 

type  file_group = 
  { dir : string ;
    sources : Bsb_db.t; 
    resources : string list ;
    public : public ;
    dir_index : Bsb_dir_index.t  ;
    generators : build_generator list ; 
    (* output of [generators] should be added to [sources],
       if it is [.ml,.mli,.re,.rei]
    *)
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

let (//) = Ext_path.combine

let (|?)  m (key, cb) =
  m  |> Ext_json.test key cb 


let warning_unused_file : _ format = 
  "@{<warning>IGNORED@}: file %s under %s is ignored because it can't be turned into a valid module name. The build system transforms a file name into a module name by upper-casing the first letter@."

type cxt = {
  not_dev : bool ;
  dir_index : Bsb_dir_index.t ; 
  cwd : string ;
  root : string;
  cut_generators : bool;
  traverse : bool
}

let collect_pub_modules 
    (xs : Ext_json_types.t array)
    (cache : Bsb_db.t) : String_set.t = 
  let set = ref String_set.empty in 
  for i = 0 to Array.length xs - 1 do 
    let v = Array.unsafe_get xs i in 
    match v with 
    | Str { str ; loc }
      -> 
      if String_map.mem str cache then 
        set := String_set.add str !set
      else 
        begin 
          Bsb_log.warn
            "@{<warning>IGNORED@} %S in public is ignored since it is not\
             an existing module@." str
        end  
    | _ -> 
      Bsb_exception.errorf 
        ~loc:(Ext_json.loc_of v)
        "public excpect a list of strings"
  done  ;
  !set
(* String_set.of_list (Bsb_build_util.get_list_string xs) *)

let  handle_list_files acc
    ({ cwd = dir ; root} : cxt)  
    loc_start loc_end 
    is_input_or_output
  : Ext_file_pp.interval list * _ =    
  (** detect files to be populated later  *)
  let files_array = readdir (Filename.concat root dir)  in 
  let dyn_file_array = String_vec.make (Array.length files_array) in 
  let files  =
    Array.fold_left (fun acc name -> 
        if is_input_or_output name then acc 
        else
          match Ext_string.is_valid_source_name name with 
          | Good ->   begin 
              let new_acc = Bsb_db.map_update ~dir acc name  in 
              String_vec.push name dyn_file_array ;
              new_acc 
            end 
          | Invalid_module_name ->
            Bsb_log.warn
              warning_unused_file name dir ;
            acc 
          | Suffix_mismatch -> acc 
      ) acc files_array in 
  [ Ext_file_pp.patch_action dyn_file_array 
      loc_start loc_end
  ],
  files






let empty = { files = []; intervals  = []; globbed_dirs = [];  }



let (++) (u : t)  (v : t)  = 
  if u == empty then v 
  else if v == empty then u 
  else 
    {
      files = Ext_list.append u.files  v.files ; 
      intervals = Ext_list.append u.intervals  v.intervals ; 
      globbed_dirs = Ext_list.append u.globbed_dirs  v.globbed_dirs ; 
    }

let get_input_output 
    loc_start 
    (content : Ext_json_types.t array) : string list * string list = 
  let error () = 
    Bsb_exception.errorf ~loc:loc_start {| invalid edge format, expect  ["output" , ":", "input" ]|}
  in  
  match Ext_array.find_and_split content 
          (fun x () -> match x with Str { str =":"} -> true | _ -> false )
          () with 
  | `No_split -> error ()
  | `Split (  output, input) -> 
    Ext_array.to_list_map (fun (x : Ext_json_types.t) -> 
        match x with
        | Str {str = ":"} -> 
          error ()
        | Str {str } ->           
          Some str 
        | _ -> None) output ,
    Ext_array.to_list_map (fun (x : Ext_json_types.t) -> 
        match x with
        | Str {str = ":"} -> 
          error () 
        | Str {str} -> 
          Some str (* More rigirous error checking: It would trigger a ninja syntax error *)
        | _ -> None) input



(** [dir_index] can be inherited  *)
let rec 
  parsing_simple_dir ({not_dev; dir_index;  cwd} as cxt ) dir : t =
  if not_dev && not (Bsb_dir_index.is_lib_dir dir_index)  then empty 
  else 
    parsing_source_dir_map 
      {cxt with
       cwd = cwd // Ext_filename.simple_convert_node_path_to_os_path dir
      }
      String_map.empty



(** 
   { dir : xx, files : ... } [dir] is already extracted 
   major work done in this function      
*)
and parsing_source_dir_map 
    ({ cwd =  dir; not_dev; cut_generators ; 
       traverse = cxt_traverse ;
     } as cxt )
    (input : Ext_json_types.t String_map.t) : t     
  = 
  let cur_sources : Bsb_db.module_info String_map.t ref = ref String_map.empty in
  let resources = ref [] in 
  let public = ref Export_all in (* TODO: move to {!Bsb_default} later*)
  let cur_update_queue = ref [] in 
  let cur_globbed_dirs = ref [] in 
  let generators : build_generator list ref  = ref [] in
  begin match String_map.find_opt Bsb_build_schemas.generators input with
    | Some (Arr { content ; loc_start}) ->
      (* Need check is dev build or not *)
      content 
      |> Array.iter (fun (x : Ext_json_types.t) ->
          match x with
          | Obj { map = generator; loc} ->
            begin match String_map.find_opt Bsb_build_schemas.name generator,
                        String_map.find_opt Bsb_build_schemas.edge generator
              with
              | Some (Str{str = command}), Some (Arr {content })->

                let output, input = get_input_output loc_start content in 
                if not cut_generators && not not_dev then begin 
                  generators := {input ; output ; command } :: !generators
                end;
                (* ATTENTION: Now adding source files, 
                   it may be re-added again later when scanning files (not explicit files input)
                *)
                output |> List.iter begin fun  output -> 
                  begin match Ext_string.is_valid_source_name output with
                    | Good ->
                      cur_sources := Bsb_db.map_update ~dir !cur_sources output
                    | Invalid_module_name ->                  
                      Bsb_log.warn warning_unused_file output dir 
                    | Suffix_mismatch -> ()
                  end
                end
              | _ ->
                Bsb_exception.errorf ~loc "Invalid generator format"
            end
          | _ -> Bsb_exception.errorf ~loc:(Ext_json.loc_of x) "Invalid generator format"
        )
    | Some x  -> Bsb_exception.errorf ~loc:(Ext_json.loc_of x ) "Invalid generators format"
    | None -> ()
  end
  ;
  let generators = !generators in 
  let sub_dirs_field = String_map.find_opt Bsb_build_schemas.subdirs input in 
  begin 
    match String_map.find_opt Bsb_build_schemas.files input with 
    | None ->  (* No setting on [!files]*)
      let file_array = readdir (Filename.concat cxt.root dir) in 
      (** We should avoid temporary files *)
      cur_sources := 
        Array.fold_left (fun acc name -> 
            if is_input_or_output generators name then 
              acc 
            else 
              match Ext_string.is_valid_source_name name with 
              | Good -> 
                Bsb_db.map_update  ~dir acc name 
              | Invalid_module_name ->
                Bsb_log.warn
                  warning_unused_file
                  name dir 
                ; 
                acc 
              | Suffix_mismatch ->  acc
          ) !cur_sources file_array;
      cur_globbed_dirs :=  [dir]  
    | Some (Arr {loc_start;loc_end; content = [||] }) -> 
      (* [ ] populatd by scanning the dir (just once) *) 
      let tasks, files =  
        handle_list_files !cur_sources cxt 
          loc_start loc_end (is_input_or_output  generators) in
      cur_update_queue := tasks ;
      cur_sources := files

    | Some (Arr {loc_start;loc_end; content = s }) -> 
      (* [ a,b ] populated by users themselves 
         TODO: still need check?
      *)      
      cur_sources := 
        Array.fold_left (fun acc (s : Ext_json_types.t) ->
            match s with 
            | Str {str = s} -> 
              Bsb_db.map_update ~dir acc s
            | _ -> acc
          ) !cur_sources s    
    | Some (Obj {map = m; loc} ) -> (* { excludes : [], slow_re : "" }*)
      let excludes = 
        match String_map.find_opt Bsb_build_schemas.excludes m with 
        | None -> []   
        | Some (Arr {content = arr}) -> Bsb_build_util.get_list_string arr 
        | Some x -> Bsb_exception.config_error x  "excludes expect array "in 
      let slow_re = String_map.find_opt Bsb_build_schemas.slow_re m in 
      let predicate = 
        match slow_re, excludes with 
        | Some (Str {str = s}), [] -> 
          let re = Str.regexp s  in 
          fun name -> Str.string_match re name 0 
        | Some (Str {str = s}) , _::_ -> 
          let re = Str.regexp s in   
          fun name -> Str.string_match re name 0 && not (List.mem name excludes)
        | Some x, _ -> Bsb_exception.errorf ~loc "slow-re expect a string literal"
        | None , _ -> Bsb_exception.errorf ~loc  "missing field: slow-re"  in 
      let file_array = readdir (Filename.concat cxt.root dir) in 
      cur_sources := Array.fold_left (fun acc name -> 
          if is_input_or_output generators name || not (predicate name) then acc 
          else 
            Bsb_db.map_update  ~dir acc name 
        ) !cur_sources file_array;
      cur_globbed_dirs := [dir]              

    | Some x -> Bsb_exception.config_error x "files field expect array or object "

  end;
  let cur_sources = !cur_sources in 
  input   
  |?  (Bsb_build_schemas.resources ,
       `Arr (fun s  ->
           resources := Bsb_build_util.get_list_string s 
         ))
  |? (Bsb_build_schemas.public, `Str_loc (fun s loc -> 
      if s = Bsb_build_schemas.export_all then public := Export_all else 
      if s = Bsb_build_schemas.export_none then public := Export_none else 
        Bsb_exception.errorf ~loc "invalid str for %s "  s 
    ))
  |? (Bsb_build_schemas.public, `Arr (fun s -> 
      public := Export_set (collect_pub_modules s cur_sources)
    ) )
  |> ignore ;
  let cur_file = 
    {dir = dir; 
     sources = cur_sources; 
     resources = !resources;
     public = !public;
     dir_index = cxt.dir_index ;
     generators ; 
    } in 
  let children, children_update_queue, children_globbed_dirs =     
    match sub_dirs_field, 
          cxt_traverse with 
    | None , true
    | Some (True _), _ -> 
      let root = cxt.root in 
      let parent = Filename.concat root dir in
      let res =
        readdir parent (* avoiding scanning twice *)
        |> Array.fold_left (fun origin x -> 
            if Sys.is_directory (Filename.concat parent x) then 
              parsing_simple_dir 
                (if cxt_traverse then cxt else {cxt with traverse = true})  x ++ origin 
            else origin  
          ) empty in 
      res.files, res.intervals, res.globbed_dirs 
    | None, false  
    | Some (False _), _  -> [], [], []

    | Some s, _  -> 
      let res  = parse_sources cxt s in 
      res.files ,
      res.intervals,
      res.globbed_dirs

  in 

  {
    files =  cur_file :: children;
    intervals = !cur_update_queue @ children_update_queue ;
    globbed_dirs = !cur_globbed_dirs @ children_globbed_dirs;
  } 

(* and parsing_simple_dir dir_index cwd  dir  : t = 
   parsing_source dir_index cwd (String_map.singleton Bsb_build_schemas.dir dir)
*)

and parsing_source ({not_dev; dir_index ; cwd} as cxt ) (x : Ext_json_types.t )
  : t  =
  match x with 
  | Str  { str = dir }  -> 
    parsing_simple_dir cxt dir   
  | Obj {map} ->
    let current_dir_index = 
      match String_map.find_opt Bsb_build_schemas.type_ map with 
      | Some (Str {str="dev"}) -> Bsb_dir_index.get_dev_index ()
      | Some _ -> Bsb_exception.config_error x {|type field expect "dev" literal |}
      | None -> dir_index in 
    if not_dev && not (Bsb_dir_index.is_lib_dir current_dir_index) then empty 
    else 
      let dir = 
        match String_map.find_opt Bsb_build_schemas.dir map with 
        | Some (Str{str}) -> 
          Ext_filename.simple_convert_node_path_to_os_path str 
        | Some x -> Bsb_exception.config_error x "dir expected to be a string"
        | None -> 
          Bsb_exception.config_error x
            (
            "required field :" ^ Bsb_build_schemas.dir ^ " missing" )
            
      in
      parsing_source_dir_map {cxt with dir_index = current_dir_index; cwd= cwd // dir} map
  | _ -> empty 
and  parsing_arr_sources cxt (file_groups : Ext_json_types.t array)  = 
  Array.fold_left (fun  origin x ->
      parsing_source cxt x ++ origin 
    ) empty  file_groups 

and  parse_sources ( cxt : cxt) (sources : Ext_json_types.t )  = 
  match sources with   
  | Arr file_groups -> 
    parsing_arr_sources cxt file_groups.content
  | _ -> parsing_source cxt sources



