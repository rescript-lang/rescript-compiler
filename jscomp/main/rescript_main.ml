(* Copyright (C) 2020- Hongbo Zhang, Authors of ReScript
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


let () =  Bsb_log.setup () 
let current_theme = ref "basic"
let separator = "--"
let watch_mode = ref false
let make_world = ref false 
let do_install = ref false
let force_regenerate = ref false
let bs_version_string = Bs_version.version
let print_version_string () = 
  print_string bs_version_string;
  print_newline (); 
  exit 0 
type spec = Bsb_arg.spec

let call_spec f : spec = Unit (Unit_call f )
let  unit_set_spec b : spec = Unit (Unit_set b)



let failed_annon = (fun ~rev_args -> 
    match rev_args with 
    | x :: _ -> Bsb_arg.bad_arg ("Don't know what to do with " ^ x)
    | _ -> ()
  )

(*Note that [keepdepfile] only makes sense when combined with [deps] for optimization*)

(**  Invariant: it has to be the last command of [bsb] *)
let exec_command_then_exit (type t) (command : string) : t =
  Bsb_log.info "@{<info>CMD:@} %s@." command;
  exit (Sys.command command ) 

(* Execute the underlying ninja build call, then exit (as opposed to keep watching) *)
let ninja_command_exit (type t) (ninja_args : string array)  : t =
  let ninja_args_len = Array.length ninja_args in
  let lib_artifacts_dir = Bsb_config.lib_bs in
  if Ext_sys.is_windows_or_cygwin then
    let path_ninja = Filename.quote Bsb_global_paths.vendor_ninja in 
    exec_command_then_exit 
      (if ninja_args_len = 0 then      
         Ext_string.inter3
           path_ninja "-C" lib_artifacts_dir
       else   
         let args = 
           Array.append 
             [| path_ninja ; "-C"; lib_artifacts_dir|]
             ninja_args in 
         Ext_string.concat_array Ext_string.single_space args)
  else
    let ninja_common_args = [|"ninja.exe"; "-C"; lib_artifacts_dir |] in 
    let args = 
      if ninja_args_len = 0 then ninja_common_args else 
        Array.append ninja_common_args ninja_args in 
    Bsb_log.info_args args ;      
    Unix.execvp Bsb_global_paths.vendor_ninja args      



(**
   Cache files generated:
   - .bsdircache in project root dir
   - .bsdeps in builddir

   What will happen, some flags are really not good
   ninja -C _build
*)
let clean_usage = "Usage : rescript.exe clean <options>\n\
                   It only clean the current project by default"
let build_usage =  "Usage : rescript.exe build <options> -- <ninja_options>\n\
                    It only builds the current project by default\n\
                    For ninja options, try rescript.exe --  -h.  \n\
                    Note they are supposed to be internals and not reliable."




let install_target () =
  let (//) = Filename.concat in  
  let vendor_ninja = Bsb_global_paths.vendor_ninja in 
  let install_dir = "lib" // "ocaml" in 
  Bsb_build_util.mkp install_dir;
  let install_command = {
    Bsb_unix.cmd = vendor_ninja ; 
    cwd =  install_dir;
    args = [| vendor_ninja ; "-f"; ".."//"bs"//"install.ninja"|]
  } in 
  let eid =
    Bsb_unix.run_command_execv
      install_command in 
  if eid <> 0 then   
    Bsb_unix.command_fatal_error install_command eid  

let global_help () =  
  print_string {|Available flags
-v, -version  display version number
-h, -help     display help 
Subcommands:
  build    
  clean
  format
  help
Run rescript subcommand -h for more details, for example
  rescript build -h
|}     

let build_subcommand ~start  argv argv_len =
  let i =  Ext_array.rfind_with_index argv Ext_string.equal separator in   

  Bsb_arg.parse_exn 
    ~usage:build_usage ~start ?finish:(if i < 0 then None else Some i )~argv [|
    "-watch", unit_set_spec watch_mode, 
    "Watch mode";
    "-with-deps", unit_set_spec make_world,
    "Build with deps";
    "-install", unit_set_spec do_install,
    "Install public interface files for dependencies ";
    "-regen", unit_set_spec force_regenerate,
    "*internal* \n\
     Always regenerate build.ninja no matter bsconfig.json is changed or not";
    "-verbose", call_spec Bsb_log.verbose,
    "Set the output to be verbose";

  |] failed_annon;

  let ninja_args = 
    if i < 0 then [||] else (Array.sub argv (i + 1) (argv_len - i - 1)) in 
  begin match ninja_args with 
    | [|"-h"|] -> ninja_command_exit ninja_args
    | _ -> 
      let config_opt = 
        Bsb_ninja_regen.regenerate_ninja
          ~package_kind:Toplevel 
          ~per_proj_dir:Bsb_global_paths.cwd
          ~forced:!force_regenerate in 
      if !make_world then 
        Bsb_world.make_world_deps Bsb_global_paths.cwd config_opt ninja_args;
      if !do_install then 
        install_target ();
      if !watch_mode then 
        exit 0 (* let the watcher do the build*)
      else ninja_command_exit ninja_args    
  end  

let clean_subcommand ~start argv =
  Bsb_arg.parse_exn 
    ~usage:clean_usage ~start ~argv [|
    "-with-deps", unit_set_spec make_world,
    "clean its deps too"
  |] failed_annon;
  if !make_world then 
    Bsb_clean.clean_bs_deps Bsb_global_paths.cwd ; 
  Bsb_clean.clean_self Bsb_global_paths.cwd      

let init_subcommand ~start argv =   
  Bsb_arg.parse_exn 
    ~usage:"init the project" ~start ~argv [|
  |] (fun 
       ~rev_args -> 
       let location = 
         match rev_args with 
         | x :: _  ->
           x 
         | [] -> 
           "." in  
       Bsb_theme_init.init_sample_project 
         ~cwd:Bsb_global_paths.cwd
         ~theme:!current_theme location
     )  
(* see discussion #929, if we catch the exception, we don't have stacktrace... *)
let () =  
  let argv = Sys.argv in   
  let argv_len = Array.length argv in   
  try
    if argv_len = 1 then begin 
      (* specialize this path which is used in watcher *)
      Bsb_ninja_regen.regenerate_ninja 
        ~package_kind:Toplevel 
        ~forced:false 
        ~per_proj_dir:Bsb_global_paths.cwd  |> ignore;
      ninja_command_exit  [||] 
    end else



      match argv.(1) with 
      | "-version" | "-v"
        -> print_version_string ()
      | "-help" | "-h"
        -> global_help ()
      | "build" -> 
        build_subcommand ~start:2 argv argv_len
      | "clean" ->         
        clean_subcommand ~start:2 argv      
      | "init" ->  
        init_subcommand ~start:2 argv 
      | "help" -> global_help ()
      | first_arg -> 
        prerr_endline @@ "Unknown subcommand or flags: " ^ first_arg;
        exit 1
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
  | Bsb_arg.Bad s 
  | Sys_error s -> 
    Format.fprintf Format.err_formatter
      "@{<error>Error:@} %s@."
      s ;
    exit 2
  | e -> Ext_pervasives.reraise e 
