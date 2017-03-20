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




let bsppx_exe = "bsppx.exe"
let bsdeps = ".bsdeps"

let (//) = Ext_filename.combine

let force_regenerate = ref false
let exec = ref false

let cwd = Sys.getcwd ()

let node_lit = "node"



let watch_exit () =
  print_endline "\nStart Watching now ";
  let bsb_watcher =
    Bsb_build_util.get_bsc_dir cwd // "bsb_watcher.js" in
  if Ext_sys.is_windows_or_cygwin then
    exit (Sys.command (Ext_string.concat3 node_lit Ext_string.single_space (Filename.quote bsb_watcher)))
  else
    Unix.execvp node_lit
      [| node_lit ;
         bsb_watcher
      |]

let no_dev = "-no-dev"
let regen = "-regen"
let separator = "--"


let internal_package_specs = "-internal-package-specs"
let internal_install = "-internal-install"

let install_targets cwd (config : Bsb_config_types.t option) =
  match config with 
  | None -> ()
  | Some {files_to_install} -> 
    let destdir = cwd // Bsb_config.lib_ocaml in
    if not @@ Sys.file_exists destdir then begin Unix.mkdir destdir 0o777  end;
    begin

      String_hash_set.iter (fun x ->
          Format.fprintf Format.std_formatter "@{<info>Trying to Installing %s@} @." x ;
          Bsb_file.install_if_exists ~destdir (cwd // x ^  Literals.suffix_ml) ;
          Bsb_file.install_if_exists ~destdir (cwd // x ^  Literals.suffix_re) ;
          Bsb_file.install_if_exists ~destdir (cwd // x ^ Literals.suffix_mli) ;
          Bsb_file.install_if_exists ~destdir (cwd // x ^  Literals.suffix_rei) ;
          Bsb_file.install_if_exists ~destdir (cwd // Bsb_config.lib_bs//x ^ Literals.suffix_cmi) ;
          Bsb_file.install_if_exists ~destdir (cwd // Bsb_config.lib_bs//x ^ Literals.suffix_cmj) ;
          Bsb_file.install_if_exists ~destdir (cwd // Bsb_config.lib_bs//x ^ Literals.suffix_cmt) ;
          Bsb_file.install_if_exists ~destdir (cwd // Bsb_config.lib_bs//x ^ Literals.suffix_cmti) ;
        ) files_to_install
    end

let build_bs_deps  deps  =
  let package_specs = 
    (String_set.fold
       (fun k acc -> k ^ "," ^ acc ) deps Ext_string.empty )  in 
  let bsc_dir = Bsb_build_util.get_bsc_dir cwd in
  let bsb_exe = bsc_dir // "bsb.exe" in
  Bsb_build_util.walk_all_deps  cwd
    (fun {top; cwd} ->
       if not top then
         Bsb_unix.run_command_execv
           {cmd = bsb_exe;
            cwd = cwd;
            args  =
              [| bsb_exe ;
                 internal_install ; 
                 no_dev; 
                 internal_package_specs; 
                 package_specs;
                 regen;
                 separator |]})

let build_bs_deps_dry_run deps =
  let bsc_dir = Bsb_build_util.get_bsc_dir cwd in
  let vendor_ninja = bsc_dir // "ninja.exe" in 
  Bsb_build_util.walk_all_deps  cwd
    (fun {top; cwd} ->
       if not top then
         begin
           Bsb_build_util.mkp (cwd // Bsb_config.lib_bs);
           let config = 
             Bsb_config_parse.interpret_json 
               ~override_package_specs:(Some deps)
               ~generate_watch_metadata:false
               ~bsc_dir
               ~no_dev:true
               cwd in 
           Bsb_config_parse.merlin_file_gen ~cwd
             (bsc_dir // bsppx_exe, 
              bsc_dir // Literals.reactjs_jsx_ppx_exe) config;
           Bsb_gen.output_ninja ~cwd ~bsc_dir config ;
           Bsb_unix.run_command_execv
             {cmd = vendor_ninja;
              cwd = cwd;
              args  = [|vendor_ninja ; "-C";Bsb_config.lib_bs|]
             };

           install_targets cwd (Some config)
         end

    )


let annoymous filename =
  String_vec.push  filename targets


let watch_mode = ref false

type make_world_config = {
  mutable  set : bool ;
  mutable dry_run : bool 
}

let make_world = {
  set = false ;
  dry_run = false;
}

(* let make_world_dry_run = ref false  *)

let set_make_world () = 
  make_world.set <- true

let set_make_world_dry_run () = 
  make_world.set <- true ; 
  make_world.dry_run <- true 


let color_enabled = ref (Unix.isatty Unix.stdin)
let set_color ppf =
  Format.pp_set_formatter_tag_functions ppf 
    ({ (Format.pp_get_formatter_tag_functions ppf () ) with
       mark_open_tag = (fun s ->  if !color_enabled then  Ext_color.ansi_of_tag s else Ext_string.empty) ;
       mark_close_tag = (fun _ ->  if !color_enabled then Ext_color.reset_lit else Ext_string.empty);
     })

let () = 
  begin 
    Format.pp_set_mark_tags Format.std_formatter true ;
    Format.pp_set_mark_tags Format.err_formatter true;
    Format.pp_set_mark_tags Format.str_formatter true;
    set_color Format.std_formatter ; 
    set_color Format.err_formatter;
    set_color Format.str_formatter
  end



let clean_bs_garbage cwd =
  Format.fprintf Format.std_formatter "@{<info>Cleaning @} in %s@." cwd ; 
  let aux x =
    let x = (cwd // x)  in
    if Sys.file_exists x then
      Bsb_unix.remove_dir_recursive x  in
  try
    List.iter aux Bsb_config.all_lib_artifacts

  with
    e ->
    Format.fprintf Format.err_formatter "@{<warning>Failed@} to clean due to %s" (Printexc.to_string e)


let clean_bs_deps () =
  Bsb_build_util.walk_all_deps  cwd  (fun {top; cwd} ->
      clean_bs_garbage cwd
    )

let clean_self () = clean_bs_garbage cwd


let bsb_main_flags : (string * Arg.spec * string) list=
  [
    "-color", Arg.Set color_enabled,
    " forced color output"
    ;
    "-no-color", Arg.Clear color_enabled,
    " forced no color output";
    "-w", Arg.Set watch_mode,
    " Watch mode" ;
    internal_install, Arg.Set Bsb_config.install,
    " (internal)Install public interface or not, when make-world it will install(in combination with -regen to make sure it has effect)";
    no_dev, Arg.Set Bsb_config.no_dev,
    " (internal)Build dev dependencies in make-world and dev group(in combination with -regen)";
    regen, Arg.Set force_regenerate,
    " (internal)Always regenerate build.ninja no matter bsconfig.json is changed or not (for debugging purpose)"
    ;
    internal_package_specs, Arg.String Bsb_config.cmd_override_package_specs,
    " (internal)Overide package specs (in combination with -regen)";
    "-clean-world", Arg.Unit clean_bs_deps,
    " Clean all bs dependencies";
    "-clean", Arg.Unit clean_self,
    " Clean only current project";
    "-make-world", Arg.Unit set_make_world,
    " Build all dependencies and itself ";
    "-make-world-dry-run", Arg.Unit set_make_world_dry_run,
    " (internal) Debugging utitlies"
  ]

(** Regenerate ninja file and return None if we dont need regenerate
    otherwise return some info
*)
let regenerate_ninja cwd bsc_dir forced =
  let output_deps = Bsb_config.lib_bs // bsdeps in
  let reason : Bsb_dep_infos.check_result =
    Bsb_dep_infos.check ~cwd  forced output_deps in
  begin match reason  with 
    | Good -> None  (* Fast path *)
    | Bsb_forced 
    | Bsb_bsc_version_mismatch 
    | Bsb_file_not_exist 
    | Bsb_source_directory_changed  
    | Other _ -> 
      print_string "Regenerating build spec : ";
      print_endline (Bsb_dep_infos.to_str reason) ; 
      if reason = Bsb_bsc_version_mismatch then begin 
        print_endline "Also clean current repo due to we have detected a different compiler";
        clean_self (); 
      end ; 
      Bsb_build_util.mkp (cwd // Bsb_config.lib_bs); 
      let config = 
        Bsb_config_parse.interpret_json 
          ~override_package_specs:!Bsb_config.cmd_package_specs
          ~bsc_dir
          ~generate_watch_metadata:true
          ~no_dev:!Bsb_config.no_dev
          cwd in 
      begin 
        Bsb_config_parse.merlin_file_gen ~cwd
          (bsc_dir // bsppx_exe, 
           bsc_dir // Literals.reactjs_jsx_ppx_exe) config;
        Bsb_gen.output_ninja ~cwd ~bsc_dir config ; 
        Literals.bsconfig_json :: config.globbed_dirs
        |> List.map
          (fun x ->
             { Bsb_dep_infos.dir_or_file = x ;
               stamp = (Unix.stat x).st_mtime
             }
          )
        |> (fun x -> Bsb_dep_infos.store ~cwd output_deps (Array.of_list x));
        Some config 
      end 
  end


let print_string_args (args : string array) =
  for i  = 0 to Array.length args - 1 do
    print_string (Array.unsafe_get args i) ;
    print_string Ext_string.single_space;
  done ;
  print_newline ()


(* Note that [keepdepfile] only makes sense when combined with [deps] for optimizatoin
   It has to be the last command of [bsb]
*)
let exec_command_install_then_exit cwd config install command =
  print_endline command ;
  let exit_code = (Sys.command command ) in
  if exit_code <> 0 then begin
    exit exit_code
  end else begin
    if install then begin  install_targets cwd config end;
    exit 0;
  end
let ninja_command_exit (type t) cwd vendor_ninja ninja_args  config : t =
  let ninja_args_len = Array.length ninja_args in
  if ninja_args_len = 0 then
    begin
      match !Bsb_config.install, Ext_sys.is_windows_or_cygwin with
      | false, false ->
        let args = [|"ninja.exe"; "-C"; Bsb_config.lib_bs |] in
        print_string_args args ;
        Unix.execvp vendor_ninja args
      | install, _ ->
        exec_command_install_then_exit cwd config install @@ Ext_string.inter3  (Filename.quote vendor_ninja) "-C" Bsb_config.lib_bs
    end
  else
    let fixed_args_length = 3 in
    begin match !Bsb_config.install, Ext_sys.is_windows_or_cygwin with
      | false, false ->
        let args = (Array.init (fixed_args_length + ninja_args_len)
                      (fun i -> match i with
                         | 0 -> "ninja.exe"
                         | 1 -> "-C"
                         | 2 -> Bsb_config.lib_bs
                         | _ -> Array.unsafe_get ninja_args (i - fixed_args_length) )) in
        print_string_args args ;
        Unix.execvp vendor_ninja args
      | install, _ ->
        let args = (Array.init (fixed_args_length + ninja_args_len)
                      (fun i -> match i with
                         | 0 -> (Filename.quote vendor_ninja)
                         | 1 -> "-C"
                         | 2 -> Bsb_config.lib_bs
                         | _ -> Array.unsafe_get ninja_args (i - fixed_args_length) )) in
        exec_command_install_then_exit cwd config install @@ Ext_string.concat_array Ext_string.single_space args
    end



(**
   Cache files generated:
   - .bsdircache in project root dir
   - .bsdeps in builddir

   What will happen, some flags are really not good
   ninja -C _build
*)
let usage = "Usage : bsb.exe <bsb-options> -- <ninja_options>\n\
             For ninja options, try ninja -h \n\
             ninja will be loaded either by just running `bsb.exe' or `bsb.exe .. -- ..`\n\
             It is always recommended to run ninja via bsb.exe \n\
             Bsb options are:"

let handle_anonymous_arg arg =
  raise (Arg.Bad ("Unknown arg \"" ^ arg ^ "\""))

let make_world_deps (config : Bsb_config_types.t option) =
  print_endline "\nMaking the dependency world!";
  let deps =
    match config with
    | None ->
      (* When this running bsb does not read bsconfig.json,
         we will read such json file to know which [package-specs]
         it wants
      *)
      Bsb_config_parse.package_specs_from_bsconfig ()
    | Some {package_specs} -> package_specs in
  if make_world.dry_run then 
    build_bs_deps_dry_run deps 
  else 
    build_bs_deps deps
    

let () =
  let bsc_dir = Bsb_build_util.get_bsc_dir cwd in
  let vendor_ninja = bsc_dir // "ninja.exe" in
  (* try *)
  (* see discussion #929 *)
  if Array.length Sys.argv <= 1 then
    begin
      let config_opt =  regenerate_ninja cwd bsc_dir false in 
      ninja_command_exit cwd vendor_ninja [||] config_opt
    end
  else
    begin
      match Ext_array.find_and_split Sys.argv Ext_string.equal separator with
      | `No_split
        ->
        begin
          Arg.parse bsb_main_flags handle_anonymous_arg usage;
          (* [-make-world] should never be combined with [-package-specs] *)
          let make_world = make_world.set in 
          begin match make_world, !force_regenerate with
            | false, false -> 
              if !watch_mode then begin
                watch_exit ()
                (* ninja is not triggered in this case
                   There are several cases we wish ninja will not be triggered.
                   [bsb -clean-world]
                   [bsb -regen ]
                *)
              end 
            | make_world, force_regenerate ->
              (* don't regenerate files when we only run [bsb -clean-world] *)
              let config_opt = regenerate_ninja cwd bsc_dir force_regenerate in
              if make_world then begin
                make_world_deps config_opt
              end;
              if !watch_mode then begin
                watch_exit ()
                (* ninja is not triggered in this case
                   There are several cases we wish ninja will not be triggered.
                   [bsb -clean-world]
                   [bsb -regen ]
                *)
              end else if make_world then begin
                ninja_command_exit cwd vendor_ninja [||] config_opt
              end
          end;

        end
      | `Split (bsb_args,ninja_args)
        -> (* -make-world all dependencies fall into this category *)
        begin
          Arg.parse_argv bsb_args bsb_main_flags handle_anonymous_arg usage ;
          let config_opt = regenerate_ninja cwd bsc_dir !force_regenerate in
          (* [-make-world] should never be combined with [-package-specs] *)
          if make_world.set then
            make_world_deps config_opt ;
          if !watch_mode then watch_exit ()
          else ninja_command_exit cwd vendor_ninja ninja_args config_opt
        end
    end
(*with x ->
  prerr_endline @@ Printexc.to_string x ;
  exit 2*)
(* with [try, with], there is no stacktrace anymore .. *)
