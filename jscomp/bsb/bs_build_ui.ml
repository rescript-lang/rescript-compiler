type 'a file_group = 
  { dir : string ;
    sources : 'a ; 
    resources : string list 
  } 

let (//) = Ext_filename.combine

let (|?)  m (key, cb) =
    m  |> Bs_json.test key cb 

let get_list_string s = 
  Ext_array.to_list_map (fun (x : Bs_json.t) ->
      match x with 
      | `Str x -> Some x 
      | _ -> None
    ) s   


let print_arrays file_array oc offset  =
  let indent = String.make offset ' ' in 
  let p_str s = 
    output_string oc indent ; 
    output_string oc s ;
    output_string oc "\n"
  in
  match file_array with 
  | []
    -> output_string oc "[ ]\n"
  | first::rest 
    -> 
    output_string oc "[ \n";
    p_str ("\"" ^ first ^ "\"");
    List.iter 
      (fun f -> 
         p_str (", \"" ^f ^ "\"")
      ) rest;
    p_str "]" 

let  handle_list_files dir s loc_start loc_end : Ext_file_pp.interval list * Binary_cache.t =  
  if Array.length s  = 0 then 
    begin 
      let files_array = Bs_dir.readdir dir  in 
      let files, file_array =
        Array.fold_left (fun (acc, f) name -> 
            let new_acc = Binary_cache.map_update ~dir acc name in 
            if new_acc == acc then 
              new_acc, f 
            else new_acc, name :: f 
          ) (String_map.empty, []) files_array in 
        [{Ext_file_pp.loc_start ;
         loc_end; action = (`print (print_arrays file_array))}],
       files
    end

  else 
    [],
     Array.fold_left (fun acc s ->
        match s with 
        | `Str s -> 
          Binary_cache.map_update ~dir acc s
        | _ -> acc
      ) String_map.empty s

(* we need add a new line in the end,
   otherwise it will be idented twice
*)
type t = 
  { files : Binary_cache.t file_group list ; 
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

let  parsing_sources cwd (file_groups : Bs_json.t array)  = 
  let rec expect_file_group cwd (x : Bs_json.t String_map.t )
    : t =
    let dir = ref cwd in
    let sources = ref String_map.empty in
    let resources = ref [] in 

    let update_queue = ref [] in 
    let globbed_dirs = ref [] in 

    let children = ref [] in 
    let children_update_queue = ref [] in 
    let children_globbed_dirs = ref [] in 
    let () = 
      x 
      |?  (Bs_build_schemas.dir, `Str (fun s -> dir := cwd // s))
      |?  (Bs_build_schemas.files ,
           `Arr_loc (fun s loc_start loc_end ->
               let dir = !dir in 
               let tasks, files =  handle_list_files  dir s loc_start loc_end in
               update_queue := tasks ;
               sources := files

             ))
      |?  (Bs_build_schemas.resources ,
           `Arr (fun s  ->
               resources := get_list_string s
             ))

      |? (Bs_build_schemas.files, 
          `Obj (fun m -> 
              let excludes = ref [] in 
              m
              |? (Bs_build_schemas.excludes, `Arr (fun arr ->  excludes := get_list_string arr))
              |? (Bs_build_schemas.slow_re, `Str 
                    (fun s -> 
                       let re = Str.regexp s in 
                       let dir = !dir in 
                       let excludes = !excludes in 
                       let file_array = Bs_dir.readdir dir in 
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
      |? (Bs_build_schemas.subdirs, `Arr (fun s -> 
          let res  = 
            Array.fold_left (fun  origin json ->
                match json with 
                | `Obj m -> 
                   expect_file_group !dir  m  ++ origin
                | _ -> origin ) empty s in 
          children :=  res.files ; 
          children_update_queue := res.intervals;
          children_globbed_dirs := res.globbed_dirs
        ))
      |> ignore 
    in 
    {
      files = {dir = !dir; sources = !sources; resources = !resources} :: !children;
      intervals = !update_queue @ !children_update_queue ;
     globbed_dirs = !globbed_dirs @ !children_globbed_dirs;
    } in 
  Array.fold_left (fun  origin x ->
      match x with 
      | `Obj map ->  
        expect_file_group cwd map ++ origin
      | _ -> origin
    ) empty  file_groups 

