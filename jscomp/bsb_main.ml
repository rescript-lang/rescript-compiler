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
let (//) = Ext_filename.combine
let get_list_string s = 
  Ext_array.to_list_map (fun (x : Bs_json.t) ->
      match x with 
      | `Str x -> Some x 
      | _ -> None
    ) s   

let bs_file_groups = ref []

(* assume build dir is fixed to be _build *)
let rel_dir = Filename.parent_dir_name 

(* More tests needed *)
let convert_unix_path_to_windows p = 
  String.map (function '/' ->'\\' | c -> c ) p 

let lazy_src_root_dir = "$src_root_dir" 
(* we use lazy $src_root_dir *)
let convert_path = 
  if Sys.unix then fun (p : string) -> 
    if Filename.basename p = p then p else 
      lazy_src_root_dir // p 
  else 
  if Sys.win32 || Sys.cygwin then 
    fun (p:string) -> 
      if Filename.basename p = p then p else 
       lazy_src_root_dir // convert_unix_path_to_windows p
  else failwith ("Unknown OS :" ^ Sys.os_type)
(* we only need convert the path in the begining*)




module Default : sig
  val set_bsc : string -> unit 
  val set_builddir : string -> unit 
  val set_bsdep : string -> unit 
  val set_ocamllex : string -> unit 
  val set_package_name : string -> unit
  val set_bs_external_includes : Bs_json.t array -> unit 
  val set_bsc_flags : Bs_json.t array -> unit 
  (* val ppx_flags : string list ref  *)

  val get_bsc : unit -> string 
  val get_builddir : unit ->  string 
  val get_bsdep : unit -> string 
  val get_ocamllex : unit -> string 
  val get_package_name : unit -> string option 
  val get_bs_external_includes : unit -> string list 
  val get_bsc_flags : unit -> string list
  val get_ppx_flags : unit -> string list 
end  = struct

  (* let () = print_endline Sys.executable_name *)
  let bsb_dir = Filename.dirname Sys.executable_name
  let bsc = ref  (bsb_dir // "bsc.exe")

  let bsdep = ref (bsb_dir // "bsdep.exe")
  let ocamllex =  ref ( "ocamllex.opt")

  let bs_external_includes = ref []


  let package_name = ref None
  let bsc_flags = ref []
  let ppx_flags = ref []
  let static_resources = ref []
  let builddir = ref "_build"

  let set_bs_external_includes s = 
    bs_external_includes := List.map convert_path (get_list_string s )
  let set_bsc_flags s = bsc_flags := get_list_string s 
  let set_bsc s = bsc := convert_path s
  let set_builddir s = builddir := convert_path s 
  let set_bsdep s = bsdep := convert_path s
  let set_ocamllex s = ocamllex := convert_path s 
  let set_package_name s = package_name := Some s
  let get_bsdep () = !bsdep
  let get_bsc () = !bsc 
  let get_builddir () = !builddir
  let get_package_name () = !package_name
  let get_ocamllex () = !ocamllex 
  let get_builddir () = !builddir
  let get_bs_external_includes () = !bs_external_includes
  let get_bsc_flags () = !bsc_flags 
  let get_ppx_flags () = !ppx_flags
end

let output_ninja 
    bsc
    bsdep
    package_name
    ocamllex
    builddir
    bs_external_includes
    bs_file_groups 
    bsc_flags
    ppx_flags 
  = 
  let eager_src_root_dir  =  Sys.getcwd () in

  let ppx_flags =
    String.concat " " @@
    Ext_list.flat_map (fun x -> ["-ppx";  x ])  ppx_flags in 
  let bs_files,source_dirs,static_resources  = 
    List.fold_left (fun (acc,dirs,acc_resources) {Bs_build_ui.sources ; dir; resources } -> 
      String_map.merge (fun modname k1 k2 ->
          match k1 , k2 with
          | None , None -> 
            assert false
          | Some a, Some b  -> 
            failwith ("conflict files found: " ^ modname)
          | Some v, None  -> Some v 
          | None, Some v ->  Some v 
        ) acc  sources , dir::dirs , (List.map (fun x -> dir // x ) resources) @ acc_resources
    ) (String_map.empty,[],[]) bs_file_groups in
  if not (Sys.file_exists builddir && Sys.is_directory builddir) then 
    begin 
      ignore @@ Unix.mkdir builddir 0o777
    end;
  Binary_cache.write_build_cache (builddir // Binary_cache.bsbuild_cache) bs_files ;
  let internal_includes =
      source_dirs
      |> Ext_list.flat_map (fun x -> ["-I" ;  x ]) (* it is a mirror, no longer need `builddir//` *)
  in 
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
      Bs_ninja.output_kvs 
        [
          "src_root_dir", eager_src_root_dir (* TODO: need check its integrity*);
          "bsc", bsc ; 
          "bsdep", bsdep; 
          "ocamllex", ocamllex;
          "bsc_computed_flags", bsc_computed_flags ; 
          "bsc_parsing_flags", bsc_parsing_flags ; 
          "ppx_flags", ppx_flags;
          "builddir", builddir;

        ]
    in
    let all_deps, all_cmis = String_map.fold
        (fun _k v acc -> 
        Bs_ninja.handle_module_info  oc v acc) bs_files ([],[]) in
    let all_deps = 
      static_resources 
      |> List.fold_left (fun all_deps x -> 
          Bs_ninja.output_build oc
            ~output:x
            ~input:(lazy_src_root_dir//x)
            ~rule:Bs_ninja.Rules.copy_resources;
          x:: all_deps 
        ) all_deps in 
    Bs_ninja.phony oc ~order_only_deps:all_deps 
      ~inputs:[]
      ~output:main_ninja ; 
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
      |?  (Bs_build_schemas.name, `Str Default.set_package_name)
      |?
      (Bs_build_schemas.ocaml_config,   `Obj  begin fun m ->
          m
          |?  (Bs_build_schemas.bsc,  `Str  Default.set_bsc)
          (* |?  (Bs_build_schemas.bsbuild,   `Str Default.set_bsbuild) *)
          |?  (Bs_build_schemas.bsdep,  `Str  Default.set_bsdep)
          |?  (Bs_build_schemas.ocamllex, `Str Default.set_ocamllex)
          (* More design *)
          |?  (Bs_build_schemas.bs_external_includes,
               `Arr Default.set_bs_external_includes)
          |?  (Bs_build_schemas.bsc_flags, `Arr Default.set_bsc_flags)

          (* More design *)
          (* |?  (Bs_build_schemas.ppx_flags, `Arr (fun s -> Default.ppx_flags := get_list_string s)) *)


          (* |?  (Bs_build_schemas.bs_copy_or_symlink, `Arr Default.set_static_resouces_from_array) *)

          |?  (Bs_build_schemas.sources, `Arr (fun xs ->
              let res =  Bs_build_ui.parsing_sources Filename.current_dir_name xs  in
              bs_file_groups := res.files ; 
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
             (get_bsc ())
             (get_bsdep ())
             (get_package_name ())
             (get_ocamllex ())
             (get_builddir ())
             (get_bs_external_includes ())
             !bs_file_groups
             (get_bsc_flags ())
             (get_ppx_flags ())
          );
  !globbed_dirs




let load_ninja argv = 
  let ninja_flags = (Array.sub Sys.argv 1 (Array.length argv - 1)) in
  Unix.execvp ninja
    (Array.concat 
       [
         [|ninja ; "-C"; (Default.get_builddir ());  "-d"; "keepdepfile"|];
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
    let builddir = Default.get_builddir () in 
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





