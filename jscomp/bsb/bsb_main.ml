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


let config_file_bak = "bsconfig.json.bak"
let ninja = "ninja"
let bsdeps = ".bsdeps"



(* Key is the path *)
let (|?)  m (key, cb) =
    m  |> Bsb_json.test key cb

let (//) = Ext_filename.combine

let bs_file_groups = ref []

(** *)
let write_ninja_file cwd =
  let builddir = Bsb_config.lib_bs in
  let () = Bsb_build_util.mkp builddir in
  let bsc, bsdep = Bsb_build_util.get_bsc_bsdep cwd  in

  let config_json_chan = open_in_bin Literals.bsconfig_json in
  let global_data = Bsb_json.parse_json_from_chan config_json_chan  in
  let update_queue = ref [] in
  let globbed_dirs = ref [] in
  let () =
    match global_data with
    | `Obj map ->
      map
      |?  (Bsb_build_schemas.name, `Str Bsb_default.set_package_name)
      |?
      (Bsb_build_schemas.ocaml_config,   `Obj  begin fun m ->
          m
          |?  (Bsb_build_schemas.ocamllex, `Str Bsb_default.set_ocamllex)
          |? (Bsb_build_schemas.bs_dependencies, `Arr Bsb_default.set_bs_dependencies)
          (* More design *)
          |?  (Bsb_build_schemas.bs_external_includes, `Arr Bsb_default.set_bs_external_includes)
          |?  (Bsb_build_schemas.bsc_flags, `Arr Bsb_default.set_bsc_flags)
          |?  (Bsb_build_schemas.ppx_flags, `Arr (Bsb_default.set_ppx_flags ~cwd))
          |?  (Bsb_build_schemas.refmt, `Str (Bsb_default.set_refmt ~cwd))
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
  Bsb_gen.output_ninja ~builddir ~cwd
             bsc
             bsdep
             (Bsb_default.get_package_name ())
             (Bsb_default.get_ocamllex ())
             (Bsb_default.get_bs_external_includes ())
             !bs_file_groups
             Bsb_default.(get_bsc_flags ())
             Bsb_default.(get_ppx_flags ())
             Bsb_default.(get_bs_dependencies ())
             Bsb_default.(get_refmt ())
          ;
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
    let output_deps = builddir // bsdeps in
    let cwd = Sys.getcwd () in

    let reason = Bsb_dep_infos.check ~cwd  output_deps in
    if String.length reason <> 0 then
      begin
        (* This is actual slow path, okay to be slight slower *)
        print_endline reason;
        print_endline "Regenrating build spec";
        let globbed_dirs = write_ninja_file cwd in
        Literals.bsconfig_json :: globbed_dirs
        |> List.map
          (fun x ->
             { Bsb_dep_infos.dir_or_file = x ;
               stamp = (Unix.stat x).st_mtime
             }
          )
        |> (fun x -> Bsb_dep_infos.store ~cwd output_deps (Array.of_list x))

      end;
    load_ninja Sys.argv

  with x ->
    prerr_endline @@ Printexc.to_string x ;
    exit 2
