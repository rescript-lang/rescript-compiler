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





(* Key is the path *)



let (|?)  m (key, cb) =
    m  |> Bs_json.test key cb 

let main_ninja = "build.ninja"
let config_file = "bsconfig.json"
let config_file_bak = "bsconfig.json.bak"
let ninja = "ninja" 
let bsdeps = ".bsdeps"
let (//) = Binary_cache.simple_concat
let get_list_string s = 
  Ext_array.to_list_map (fun (x : Bs_json.t) ->
      match x with 
      | `Str x -> Some x 
      | _ -> None
    ) s   

(* More tests needed *)
let convert_unix_path_to_windows p = 
  String.map (function '/' ->'\\' | c -> c ) p 

let convert_path  = 
  if Sys.unix then fun p -> p else 
  if Sys.win32 || Sys.cygwin then convert_unix_path_to_windows
  else failwith ("Unknown OS :" ^ Sys.os_type)
(* we only need convert the path in the begining*)




module Default = struct
  let bsc = ref  "bsc.exe"
  let bsbuild = ref "bsbuild.exe"
  let bsdep = ref "bsdep.exe"
  let ocamllex =  ref "ocamllex.opt"

  let bs_external_includes = ref []


  let package_name = ref None
  let bsc_flags = ref []
  let ppx_flags = ref []
  let static_resources = ref []
  let builddir = ref "_build"
  let bs_file_groups = ref []

  let set_bsc s = bsc := convert_path s
  let set_bsbuild s = bsbuild := convert_path s 
  let set_bsdep s = bsdep := convert_path s
  let set_ocamllex s = ocamllex := convert_path s 
  let set_static_resouces_from_array s = 
    static_resources := Ext_array.to_list_map (fun x ->
      match x with 
      | `Str x -> Some (convert_path x)
      | _ -> None) s 
end

let output_ninja 
    bsc
    bsdep
    package_name
    ocamllex
    builddir
    bs_external_includes
    static_resources 
    bs_file_groups 
    bsc_flags
    ppx_flags 
  = 
  let ppx_flags =
    String.concat " " @@
    Ext_list.flat_map (fun x -> ["-ppx";  x ])  ppx_flags in 
  let bs_files, source_dirs  = List.fold_left (fun (acc,dirs) {Bs_build_ui.sources ; dir } -> 
      String_map.merge (fun modname k1 k2 ->
          match k1 , k2 with
          | None , None -> 
            assert false
          | Some a, Some b  -> 
            failwith ("conflict files found: " ^ modname)
          | Some v, None  -> Some v 
          | None, Some v ->  Some v 
        ) acc  sources , dir::dirs
    ) (String_map.empty,[]) bs_file_groups in
  if not (Sys.file_exists builddir && Sys.is_directory builddir) then 
    begin 
      ignore @@ Unix.mkdir builddir 0o777
    end;
  Binary_cache.write_build_cache (builddir // Binary_cache.bsbuild_cache) bs_files ;
  let internal_includes =
      source_dirs
      |> Ext_list.flat_map (fun x -> ["-I" ; builddir // x ]) in 
  let external_includes = 
      Ext_list.flat_map (fun x -> ["-I" ; x]) bs_external_includes in 

  let bsc_parsing_flags =
    String.concat " " bsc_flags 
  in  
  let bsc_computed_flags =
    let init_flags = 
      match package_name with 
      | None -> external_includes @ internal_includes 
      | Some x -> "-bs-package-name" ::  x :: external_includes @ internal_includes
    in 
    String.concat " " ( bsc_flags @ init_flags)
  in
  let oc = open_out (builddir // main_ninja) in 
  begin 
    let () = 
      oc 
      |>
      Bs_ninja.output_kvs [ "bsc", bsc ; 
                   "bsc_computed_flags", bsc_computed_flags ; 
                   "bsc_parsing_flags", bsc_parsing_flags ; 
                   "bsdep", bsdep; 
                   "ocamllex", ocamllex;
                   "ppx_flags", ppx_flags;
                   "builddir", builddir
                 ]
    in
    let all_deps, all_cmis = String_map.fold
        (fun _k v acc -> 
        Bs_ninja.handle_module_info builddir oc v acc) bs_files ([],[]) in
    let all_deps = 
      static_resources 
      |> List.fold_left (fun all_deps x -> 
          let output = (builddir//x) in
          Bs_ninja.output_build oc
            ~output
            ~input:x
            ~rule:Bs_ninja.Rules.copy_resources;
          output:: all_deps 
        ) all_deps in 
    Bs_ninja.phony oc ~order_only_deps:all_deps 
      ~inputs:[]
      ~output:(builddir//main_ninja) ; 
    close_out oc;
  end




(** *)
let write_ninja_file () = 
  let config_json_chan = open_in_bin config_file in 
  let global_data = Bs_json.parse_json_from_chan config_json_chan  in
  let update_queue = ref [] in 
  let globbed_dirs = ref [] in
  let () = 
    match global_data with
    | `Obj map -> 
      map 
      |?  (Bs_build_schemas.name, `Str (fun s -> Default.package_name := Some s))
      |?
      (Bs_build_schemas.ocaml_config,   `Obj  begin fun m ->
          m
          |?  (Bs_build_schemas.bsc,  `Str  Default.set_bsc)
          (* |?  (Bs_build_schemas.bsbuild,   `Str Default.set_bsbuild) *)
          |?  (Bs_build_schemas.bsdep,  `Str  Default.set_bsdep)
          |?  (Bs_build_schemas.ocamllex, `Str Default.set_ocamllex)
          (* More design *)
          |?  (Bs_build_schemas.bs_external_includes,
               `Arr (fun s -> Default.bs_external_includes := get_list_string s))
          |?  (Bs_build_schemas.bsc_flags, `Arr (fun s -> Default.bsc_flags :=  get_list_string s ))

          (* More design *)
          |?  (Bs_build_schemas.ppx_flags, `Arr (fun s -> Default.ppx_flags := get_list_string s))


          |?  (Bs_build_schemas.bs_copy_or_symlink, `Arr Default.set_static_resouces_from_array)

          |?  (Bs_build_schemas.sources, `Arr (fun xs ->
              let res =  Bs_build_ui.parsing_sources xs  in
              Default.bs_file_groups := res.files ; 
              update_queue := res.intervals;
              globbed_dirs := res.globbed_dirs
            ))
          |> ignore
        end)
      |> ignore

    | _ -> ()
  in
  begin match List.sort Ext_file_pp.interval_compare  !update_queue with 
  | [] -> ()
  | queue -> 
    let file_size = in_channel_length config_json_chan in
    let oc = open_out_bin config_file_bak in
    let () = 
      Ext_file_pp.process_wholes
        queue file_size config_json_chan oc in 
    close_out oc ;
    close_in config_json_chan ; 
    Unix.unlink config_file; 
    Unix.rename config_file_bak config_file
  end;
  Default.(output_ninja 
             !bsc     
             !bsdep
             !package_name
             !ocamllex
             !builddir
             !bs_external_includes
             !static_resources 
             !bs_file_groups 
             !bsc_flags
             !ppx_flags 
          );
  !globbed_dirs




let load_ninja argv = 
  let ninja_flags = (Array.sub Sys.argv 1 (Array.length argv - 1)) in
  Unix.execvp ninja
    (Array.concat 
       [
         [|ninja ; "-f"; (!Default.builddir // main_ninja);  "-d"; "keepdepfile"|];
         ninja_flags
       ]
    )

(**
Cache files generated:
- .bsdircache in project root dir 
- .bsdeps in builddir 

What will happen, some flags are really not good  
ninja -C _build 
*)
let () = 
  try
    let builddir = !Default.builddir in 
    let output_deps = (builddir // bsdeps) in
    let reason = Bs_dep_infos.check  output_deps in 
    if String.length reason <> 0 then 
      begin
        (* This is actual slow path, okay to be slight slower *)
        print_endline reason;
        print_endline "Regenrating build spec";
        let globbed_dirs = write_ninja_file () in 
        config_file :: globbed_dirs 
        |> List.map
          (fun x ->
             { Bs_dep_infos.dir_or_file = x ;
               stamp = (Unix.stat x).st_mtime
             }
          ) 
        |> Array.of_list 
        |> Bs_dep_infos.write output_deps

      end;
    load_ninja Sys.argv

  with x ->
    prerr_endline @@ Printexc.to_string x ; 
    exit 2 





