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



let cwd = Sys.getcwd ()
let bsc_dir = Bsb_build_util.get_bsc_dir ~cwd 
let () =  Bsb_log.setup () 
let (//) = Ext_path.combine
let force_regenerate = ref false
let exec = ref false
let node_lit = "node"
let current_theme = ref "basic"
let set_theme s = current_theme := s 
let generate_theme_with_path = ref None
let regen = "-regen"
let separator = "--"
let watch_mode = ref false
let make_world = ref false 
let set_make_world () = make_world := true
let bs_version_string = Bs_version.version

#if BS_NATIVE then
let cmdline_backend = ref Bsb_config_types.Js

let is_cmdline_backend_set = ref false

let build_library = ref None

let get_backend () =
  (* If cmdline_backend is set we use it, otherwise we actually shadow it for the first entry. *)
  if !is_cmdline_backend_set then
    !cmdline_backend
  else
    let entries = Bsb_config_parse.entries_from_bsconfig () in 
    let b = (List.hd entries).backend in
    begin match List.hd b with
      | Bsb_config_types.JsTarget       -> Bsb_config_types.Js
      | Bsb_config_types.NativeTarget   -> Bsb_config_types.Native
      | Bsb_config_types.BytecodeTarget -> Bsb_config_types.Bytecode
    end 

let get_string_backend = function
  | Bsb_config_types.Js       -> "js"
  | Bsb_config_types.Native   -> "native"
  | Bsb_config_types.Bytecode -> "bytecode"
#end

let print_version_string () = 
  print_string bs_version_string;
  print_newline (); 
  exit 0 

let bsb_main_flags : (string * Arg.spec * string) list=
  [
    "-v", Arg.Unit print_version_string, 
    " Print version and exit";
    "-version", Arg.Unit print_version_string, 
    " Print version and exit";
    "-verbose", Arg.Unit Bsb_log.verbose,
    " Set the output(from bsb) to be verbose";
    "-w", Arg.Set watch_mode,
    " Watch mode" ;     
    "-clean-world", Arg.Unit (fun _ -> 
        Bsb_clean.clean_bs_deps bsc_dir cwd),
    " Clean all bs dependencies";
    "-clean", Arg.Unit (fun _ -> 
        Bsb_clean.clean_self bsc_dir cwd),
    " Clean only current project";
    "-make-world", Arg.Unit set_make_world,
    " Build all dependencies and itself ";
    "-init", Arg.String (fun path -> generate_theme_with_path := Some path),
    " Init sample project to get started. Note (`bsb -init sample` will create a sample project while `bsb -init .` will reuse current directory)";
    "-theme", Arg.String set_theme,
    " The theme for project initialization, default is basic(https://github.com/bucklescript/bucklescript/tree/master/jscomp/bsb/templates)";
    
    regen, Arg.Set force_regenerate,
    " (internal) Always regenerate build.ninja no matter bsconfig.json is changed or not (for debugging purpose)";
    "-query", Arg.String (fun s -> Bsb_query.query ~cwd ~bsc_dir s ),
    " (internal)Query metadata about the build";
    "-themes", Arg.Unit Bsb_theme_init.list_themes,
    " List all available themes";
#if BS_NATIVE then
    "-backend", Arg.String (fun s -> 
        is_cmdline_backend_set := true;
        match s with
        | "js"       -> cmdline_backend := Bsb_config_types.Js
        | "native"   -> cmdline_backend := Bsb_config_types.Native
        | "bytecode" -> cmdline_backend := Bsb_config_types.Bytecode
        | _ -> failwith "-backend should be one of: 'js', 'bytecode' or 'native'."
      ),
    " Builds the entries in the bsconfig which match the given backend.";
    
    "-build-artifacts-dir", Arg.String (fun s -> Bsb_build_util.build_artifacts_dir := Some (cwd // s)),
    " Sets the directory in which all the project's build artifacts will go.";

    "-build-library", Arg.String (fun main_file -> build_library := Some(main_file)),
    " Builds a static library given a main module name. Outputs a cmxa/cma file depending on -backend.";
#end
    "-where",
    Arg.Unit (fun _ -> 
        print_endline (Filename.dirname Sys.executable_name)),
    " Show where bsb.exe is located"
  ]


(*Note that [keepdepfile] only makes sense when combined with [deps] for optimization*)

(**  Invariant: it has to be the last command of [bsb] *)
let exec_command_then_exit  command =
  Bsb_log.info "@{<info>CMD:@} %s@." command;
  exit (Sys.command command ) 

(* Execute the underlying ninja build call, then exit (as opposed to keep watching) *)
#if BS_NATIVE then
let ninja_command_exit cwd vendor_ninja ninja_args nested =
  let build_ninja_path = (Bsb_build_util.get_build_artifacts_location cwd) // Bsb_config.lib_bs // nested in
#else
let ninja_command_exit  vendor_ninja ninja_args  =
  let build_ninja_path = Bsb_config.lib_bs in
#end
  let ninja_args_len = Array.length ninja_args in
  if Ext_sys.is_windows_or_cygwin then
    let path_ninja = Filename.quote vendor_ninja in 
    exec_command_then_exit @@ 
    (if ninja_args_len = 0 then      
       Ext_string.inter3
         path_ninja "-C" build_ninja_path
     else   
       let args = 
         Array.append 
           [| path_ninja ; "-C"; build_ninja_path|]
           ninja_args in 
       Ext_string.concat_array Ext_string.single_space args)
  else
    let ninja_common_args = [|"ninja.exe"; "-C"; build_ninja_path |] in 
    let args = 
      if ninja_args_len = 0 then ninja_common_args else 
        Array.append ninja_common_args ninja_args in 
    Bsb_log.info_args args ;      
#if BS_NATIVE then
    (* @SorryMatchenv We're going to remove this in the next version *)
    let environment = Unix.environment () in
    let environment = (Array.append environment [| "BSB_BACKEND=" ^ nested |]) in
    Unix.execvpe vendor_ninja args environment
#else
    Unix.execvp vendor_ninja args      
#end



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


let watch_exit () =
  exit 0

(* see discussion #929, if we catch the exception, we don't have stacktrace... *)
let () =

  let vendor_ninja = bsc_dir // "ninja.exe" in  
#if BS_NATIVE then
  let ocaml_dir = Bsb_build_util.get_ocaml_dir cwd in
#end
  try begin 
    match Sys.argv with 
#if BS_NATIVE then
    (* Both of those are equivalent and the watcher will always pass in the `-backend` flag. *)
    | [| _; "-backend"; _ |] 
#end
    | [| _ |] ->  (* specialize this path [bsb.exe] which is used in watcher *)
      begin
#if BS_NATIVE then
        let backend = if Array.length Sys.argv = 3 then begin match Array.get Sys.argv 2 with
          | "js"       -> Bsb_config_types.Js
          | "native"   -> Bsb_config_types.Native
          | "bytecode" -> Bsb_config_types.Bytecode
          | _ -> failwith "-backend should be one of: 'js', 'bytecode' or 'native'."
        end else get_backend () in
        
        let main_config = 
          Bsb_config_parse.interpret_json  ~override_package_specs:None ~bsc_dir ~generate_watch_metadata:true ~not_dev:false cwd in
        let _ = 
          Bsb_ninja_regen.regenerate_ninja ~not_dev:false 
            ~is_top_level:true
            ~root_project_dir:cwd
            ~forced:false
            ~build_library:None
            ~backend
            ~main_config
            ~ocaml_dir
            cwd bsc_dir 
        in
        let nested = get_string_backend backend in
        ninja_command_exit (Bsb_build_util.get_build_artifacts_location cwd) vendor_ninja [||] nested
#else
        let _config_opt =  
          Bsb_ninja_regen.regenerate_ninja ~override_package_specs:None ~not_dev:false 
            ~generate_watch_metadata:true
            ~forced:false 
            cwd bsc_dir 
        in 
        ninja_command_exit  vendor_ninja [||] 
#end
      end
    | argv -> 
      begin
        match Ext_array.find_and_split argv Ext_string.equal separator with
        | `No_split
          ->
          begin
            Arg.parse bsb_main_flags handle_anonymous_arg usage;
            (* first, check whether we're in boilerplate generation mode, aka -init foo -theme bar *)
            match !generate_theme_with_path with
            | Some path -> Bsb_theme_init.init_sample_project ~cwd ~theme:!current_theme path
            | None -> 
              (* [-make-world] should never be combined with [-package-specs] *)
              let make_world = !make_world in 
#if BS_NATIVE then
              let backend = get_backend () in

              begin match make_world, !force_regenerate, !build_library with
                | false, false, None -> 
#else
              begin match make_world, !force_regenerate with
                | false, false -> 
#end
                  (* [regenerate_ninja] is not triggered in this case
                     There are several cases we wish ninja will not be triggered.
                     [bsb -clean-world]
                     [bsb -regen ]
                  *)
                  if !watch_mode then begin
                    watch_exit ()
                  end 
#if BS_NATIVE then
                | make_world, force_regenerate, build_library ->
                  let main_config = 
                    Bsb_config_parse.interpret_json 
                      ~override_package_specs:None
                      ~bsc_dir
                      ~generate_watch_metadata:true
                      ~not_dev:false
                      cwd in
                  (* We have an optimization here where we can collect the `dependency_info` data
                     while traversing and building the dependencies (when `-make-world` is passed).
                     If `-make-world` is not passed, we still need to traverse the deps to collect
                     stuff like `c-static-libraries` etc...
                                  Ben - August 10th 2018   
                  *)
                  let dependency_info = if make_world then
                    Some (Bsb_world.make_world_deps cwd ~root_project_dir:cwd ~backend ~main_config)
                  else None in
                  (* don't regenerate files when we only run [bsb -clean-world] *)
                  let _did_regen = Bsb_ninja_regen.regenerate_ninja 
                    ?dependency_info 
                    ~is_top_level:true
                    ~not_dev:false 
                    ~root_project_dir:cwd
                    ~forced:force_regenerate
                    ~build_library:build_library
                    ~backend
                    ~main_config
                    ~ocaml_dir
                    cwd bsc_dir in
#else
                | make_world, force_regenerate ->
                  let config_opt = Bsb_ninja_regen.regenerate_ninja ~generate_watch_metadata:true ~override_package_specs:None ~not_dev:false ~forced:force_regenerate cwd bsc_dir  in
                  if make_world then begin
                    Bsb_world.make_world_deps cwd config_opt
                  end;
#end
                  if !watch_mode then begin
                    watch_exit ()
                    (* ninja is not triggered in this case
                       There are several cases we wish ninja will not be triggered.
                       [bsb -clean-world]
                       [bsb -regen ]
                    *)
#if BS_NATIVE then
                  end else begin
                    let nested = get_string_backend backend in
                    ninja_command_exit (Bsb_build_util.get_build_artifacts_location cwd) vendor_ninja [||] nested
#else
                  end else if make_world then begin
                    ninja_command_exit  vendor_ninja [||] 
#end
                  end
              end;
          end
        | `Split (bsb_args,ninja_args)
          -> (* -make-world all dependencies fall into this category *)
          begin
            Arg.parse_argv bsb_args bsb_main_flags handle_anonymous_arg usage ;
#if BS_NATIVE then
            let backend = get_backend () in

            let main_config = 
              Bsb_config_parse.interpret_json 
                ~override_package_specs:None
                ~bsc_dir
                ~generate_watch_metadata:true
                ~not_dev:false
                cwd in
            (* [-make-world] should never be combined with [-package-specs] *)
            let dependency_info = if !make_world then 
              Some (Bsb_world.make_world_deps cwd ~root_project_dir:cwd ~backend ~main_config)
            else None in
            let _did_regen = Bsb_ninja_regen.regenerate_ninja 
              ?dependency_info
              ~is_top_level:true
              ~not_dev:false
              ~root_project_dir:cwd
              ~forced:!force_regenerate
              ~build_library:!build_library
              ~backend
              ~main_config
              ~ocaml_dir
              cwd bsc_dir in
            if !watch_mode then watch_exit ()
            else begin 
              let nested = get_string_backend backend in
              ninja_command_exit (Bsb_build_util.get_build_artifacts_location cwd) vendor_ninja ninja_args nested
            end
#else
            let config_opt = Bsb_ninja_regen.regenerate_ninja ~generate_watch_metadata:true ~override_package_specs:None ~not_dev:false cwd bsc_dir ~forced:!force_regenerate in
            (* [-make-world] should never be combined with [-package-specs] *)
            if !make_world then
              Bsb_world.make_world_deps cwd config_opt ;
            if !watch_mode then watch_exit ()
            else ninja_command_exit  vendor_ninja ninja_args 
#end
          end
      end
  end
  with 
  | Bsb_exception.Error e ->
    Bsb_exception.print Format.err_formatter e ;
    Format.pp_print_newline Format.err_formatter ();
    exit 2
  | Ext_json_parse.Error (start,_,e) -> 
    Format.fprintf Format.err_formatter
      "File %S, line %d\n\
       @{<error>Error:@} %a@."
      start.pos_fname start.pos_lnum
      Ext_json_parse.report_error e ;
    exit 2
  | Arg.Bad s 
  | Sys_error s -> 
    Format.fprintf Format.err_formatter
      "@{<error>Error:@} %s@."
      s ;
    exit 2
  | e -> Ext_pervasives.reraise e 
