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
    m  |> Bsb_json.test key cb 



let config_file_bak = "bsconfig.json.bak"
let ninja = "ninja" 
let bsdeps = ".bsdeps"
let (//) = Ext_filename.combine
let get_list_string s = 
  Ext_array.to_list_map (fun (x : Bsb_json.t) ->
      match x with 
      | `Str x -> Some x 
      | _ -> None
    ) s   

let bs_file_groups = ref []


(* let () = print_endline Sys.executable_name *)

(* let bsb_dir =  *)
(*   let bsb_path = Filename.dirname Sys.executable_name in  *)
(*   Bs_build_util.convert_path (Bsb_config.rev_lib_bs_prefix  bsb_path) *)

module Default : sig
  (* val set_bsc : string -> unit  *)
  (* val set_builddir : string -> unit  *)
  (* val set_bsdep : string -> unit  *)
  val set_ocamllex : string -> unit 
  val set_package_name : string -> unit
  val set_bs_external_includes : Bsb_json.t array -> unit 
  val set_bsc_flags : Bsb_json.t array -> unit 
  (* val ppx_flags : string list ref  *)

  (* val get_bsc : unit -> string  *)
  (* val get_builddir : unit ->  string *)
  (* val get_bsdep : unit -> string  *)
  val get_ocamllex : unit -> string 
  val get_package_name : unit -> string option 
  val get_bs_external_includes : unit -> string list 
  val get_bsc_flags : unit -> string list
  val get_ppx_flags : unit -> string list 
  val get_bs_dependencies : unit  -> string list 
  val set_bs_dependencies : Bsb_json.t array  -> unit
end  = struct


  (* let bsc = ref  (bsb_dir // "bsc.exe") *)

  (* let bsdep = ref (bsb_dir // "bsdep.exe") *)
  let ocamllex =  ref ( "ocamllex.opt")

  let bs_external_includes = ref []


  let package_name = ref None
  let bsc_flags = ref []
  let ppx_flags = ref []
  let static_resources = ref []
  let builddir = ref ("lib"//"bs")
  let bs_dependencies = ref []
  let get_bs_dependencies () = !bs_dependencies
  let set_bs_dependencies  s =
    bs_dependencies := get_list_string s 
  let set_bs_external_includes s = 
    bs_external_includes := List.map Bsb_build_util.convert_path (get_list_string s )
  let set_builddir s = builddir := Bsb_build_util.convert_path s 

  let set_bsc_flags s = bsc_flags := get_list_string s 
  (* let set_bsc s = bsc := Bs_build_util.convert_file s *)
  (* let set_bsdep s = bsdep := Bs_build_util.convert_file s *)
  let set_ocamllex s = ocamllex := Bsb_build_util.convert_file s 
  let set_package_name s = package_name := Some s
  (* let get_bsdep () = !bsdep *)
  (* let get_bsc () = !bsc  *)
  (* let get_builddir () = !builddir *)
  let get_package_name () = !package_name
  let get_ocamllex () = !ocamllex 
  let get_builddir () = !builddir
  let get_bs_external_includes () = !bs_external_includes
  let get_bsc_flags () = !bsc_flags 
  let get_ppx_flags () = !ppx_flags
end

let output_ninja 
    ~builddir
    ~cwd 
    bsc
    bsdep
    package_name
    ocamllex
    bs_external_includes
    bs_file_groups 
    bsc_flags
    ppx_flags 
    bs_dependencies

  = 



  let ppx_flags = Bsb_build_util.flag_concat "-ppx" ppx_flags in 
  let bs_groups, source_dirs,static_resources  = 
    List.fold_left (fun (acc, dirs,acc_resources) ({Bsb_build_ui.sources ; dir; resources }) -> 
      String_map.merge (fun modname k1 k2 ->
          match k1 , k2 with
          | None , None -> 
            assert false
          | Some a, Some b  -> 
            failwith ("conflict files found: " ^ modname)
          | Some v, None  -> Some v 
          | None, Some v ->  Some v 
        ) acc  sources ,  dir::dirs , (List.map (fun x -> dir // x ) resources) @ acc_resources
    ) (String_map.empty,[],[]) bs_file_groups in
  Binary_cache.write_build_cache (builddir // Binary_cache.bsbuild_cache) bs_groups ;
  let internal_includes =
      source_dirs
      |> Ext_list.flat_map (fun x -> ["-I" ;  x ]) (* it is a mirror, no longer need `builddir//` *)
  in 
  let external_includes = 
      Ext_list.flat_map (fun x -> ["-I" ; x]) bs_external_includes in 
  let bs_package_includes = 
    Bsb_build_util.flag_concat "-bs-package-include" bs_dependencies in 
  let bsc_parsing_flags =
    String.concat " " bsc_flags 
  in  
  let bsc_computed_flags =
    let init_flags = 
      match package_name with 
      | None -> external_includes @ internal_includes 
      | Some x -> "-bs-package-name" ::  x :: external_includes @ internal_includes
    in  (* make sure -bs-package-name is before -bs-package-output *)
    String.concat " " ( bsc_flags @ init_flags)
  in
  let oc = open_out_bin (builddir // Literals.build_ninja) in 
  begin 

    let () = 
      oc 
      |>
      Bsb_ninja.output_kvs 
        [
          "src_root_dir", cwd (* TODO: need check its integrity*);

          "bsc", bsc ; 
          "bsdep", bsdep; 
          "ocamllex", ocamllex;
          "bsc_computed_flags", bsc_computed_flags ; 
          "bsc_parsing_flags", bsc_parsing_flags ; 
          "ppx_flags", ppx_flags;
          "bs_packaeg_includes", bs_package_includes;
          "bs_package_flags", "" (* TODO*)
          (* "builddir", builddir; we should not have it set, since it's correct here *)

        ]
    in
    let all_deps, all_cmis = 
      Bsb_ninja.handle_file_groups oc bs_file_groups ([],[]) in 
    let all_deps = 
      (* we need copy package.json into [_build] since it does affect build output *)
      (* Literals.package_json ::  
         it is a bad idea to copy package.json which requires to copy js files
      *)
      static_resources 
      |> List.fold_left (fun all_deps x -> 
          Bsb_ninja.output_build oc
            ~output:x
            ~input:(Bsb_config.proj_rel x)
            ~rule:Bsb_ninja.Rules.copy_resources;
          x:: all_deps 
        ) all_deps in 
    Bsb_ninja.phony oc ~order_only_deps:all_deps 
      ~inputs:[]
      ~output:Literals.build_ninja ; 
    close_out oc;
  end




(** *)
let write_ninja_file () = 
  let builddir = Bsb_config.lib_bs in 
  let cwd = Sys.getcwd () in 
  let bsc, bsdep = Bsb_build_util.get_bsc_bsdep cwd  in
  let () = Bsb_build_util.mkp builddir in 
  let config_json_chan = open_in_bin Literals.bsconfig_json in 
  let global_data = Bsb_json.parse_json_from_chan config_json_chan  in
  let update_queue = ref [] in 
  let globbed_dirs = ref [] in
  let () = 
    match global_data with
    | `Obj map -> 
      map 
      |?  (Bsb_build_schemas.name, `Str Default.set_package_name)
      |?
      (Bsb_build_schemas.ocaml_config,   `Obj  begin fun m ->
          m
          (* |?  (Bs_build_schemas.bsc,  `Str  Default.set_bsc) *)
          (* |?  (Bs_build_schemas.bsbuild,   `Str Default.set_bsbuild) *)
          (* |?  (Bs_build_schemas.bsdep,  `Str  Default.set_bsdep) *)
          |?  (Bsb_build_schemas.ocamllex, `Str Default.set_ocamllex)
          |? (Bsb_build_schemas.bs_dependencies, `Arr Default.set_bs_dependencies)
          (* More design *)
          |?  (Bsb_build_schemas.bs_external_includes,
               `Arr Default.set_bs_external_includes)
          |?  (Bsb_build_schemas.bsc_flags, `Arr Default.set_bsc_flags)

          (* More design *)
          (* |?  (Bs_build_schemas.ppx_flags, `Arr (fun s -> Default.ppx_flags := get_list_string s)) *)


          (* |?  (Bs_build_schemas.bs_copy_or_symlink, `Arr Default.set_static_resouces_from_array) *)

          |?  (Bsb_build_schemas.sources, `Arr (fun xs ->
              let res =  Bsb_build_ui.parsing_sources Filename.current_dir_name xs  in
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
    Unix.unlink Literals.bsconfig_json; 
    Unix.rename config_file_bak Literals.bsconfig_json
  end;
  Default.(output_ninja ~builddir ~cwd
             (* (get_bsc ()) *)
             (* (get_bsdep ()) *)
             bsc 
             bsdep 
             (get_package_name ())
             (get_ocamllex ())
             (get_bs_external_includes ())
             !bs_file_groups
             (get_bsc_flags ())
             (get_ppx_flags ())
             (get_bs_dependencies ())
          );
  !globbed_dirs




let load_ninja argv = 
  let ninja_flags = (Array.sub Sys.argv 1 (Array.length argv - 1)) in
  Unix.execvp ninja
    (Array.concat 
       [
         [|ninja ; "-C"; Bsb_config.lib_bs;  "-d"; "keepdepfile"|];
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
    let builddir = Bsb_config.lib_bs in 
    let output_deps = (builddir // bsdeps) in
    let reason = Bsb_dep_infos.check  output_deps in 
    if String.length reason <> 0 then 
      begin
        (* This is actual slow path, okay to be slight slower *)
        print_endline reason;
        print_endline "Regenrating build spec";
        let globbed_dirs = write_ninja_file () in 
        Literals.bsconfig_json :: globbed_dirs 
        |> List.map
          (fun x ->
             { Bsb_dep_infos.dir_or_file = x ;
               stamp = (Unix.stat x).st_mtime
             }
          ) 
        |> Array.of_list 
        |> Bsb_dep_infos.write output_deps

      end;
    load_ninja Sys.argv

  with x ->
    prerr_endline @@ Printexc.to_string x ; 
    exit 2 





