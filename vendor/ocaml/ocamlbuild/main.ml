(***********************************************************************)
(*                                                                     *)
(*                             ocamlbuild                              *)
(*                                                                     *)
(*  Nicolas Pouillard, Berke Durak, projet Gallium, INRIA Rocquencourt *)
(*                                                                     *)
(*  Copyright 2007 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)


(* Original author: Berke Durak *)
open My_std
open Log
open Pathname.Operators
open Command
open Tools
open Ocaml_specific
open Format
;;

exception Exit_build_error of string
exception Exit_silently

let clean () =
  Shell.rm_rf !Options.build_dir;
  if !Options.make_links then begin
    let entry =
      Slurp.map (fun _ _ _ -> true)
        (Slurp.slurp Filename.current_dir_name)
    in
    Slurp.force (Resource.clean_up_links entry)
  end;
  Log.finish ();
  raise Exit_silently
;;

let show_tags () =
  if List.length !Options.show_tags > 0 then
    Log.eprintf "Warning: the following tags do not include \
    dynamically-generated tags, such as link, compile, pack, byte, native, c, \
    pdf... (this list is by no means exhaustive).\n";
  List.iter begin fun path ->
    Log.eprintf "@[<2>Tags for %S:@ {. %a .}@]" path Tags.print (tags_of_pathname path)
  end !Options.show_tags
;;

let show_documentation () =
  Rule.show_documentation ();
  Flags.show_documentation ();
;;

(* these tags are used in an ad-hoc way by the ocamlbuild implementation;
   this means that even if they were not part of any flag declaration,
   they should be marked as useful, to avoid the "unused tag" warning. *)
let builtin_useful_tags =
  Tags.of_list [
    "include"; "traverse"; "not_hygienic"; "precious";
    "pack"; "ocamlmklib"; "native"; "thread";
    "nopervasives"; "use_menhir"; "ocamldep";
    "thread";
  ]
;;

let proceed () =
  Hooks.call_hook Hooks.Before_options;
  Options.init ();
  Options.include_dirs := List.map Pathname.normalize !Options.include_dirs;
  Options.exclude_dirs := List.map Pathname.normalize !Options.exclude_dirs;
  if !Options.must_clean then clean ();
  Hooks.call_hook Hooks.After_options;
  let options_wd = Sys.getcwd () in
  let first_run_for_plugin =
    (* If we are in the first run before launching the plugin, we
       should skip the user-visible operations (hygiene) that may need
       information from the plugin to run as the user expects it.

       Note that we don't need to disable the [Hooks] call as they are
       no-ops anyway, before any plugin has registered hooks. *)
    Plugin.we_need_a_plugin () && not !Options.just_plugin in

  let target_dirs = List.union [] (List.map Pathname.dirname !Options.targets) in

  Configuration.parse_string ~source:Const.Source.builtin
    "<**/*.ml> or <**/*.mli> or <**/*.mlpack> or <**/*.ml.depends>: ocaml\n\
     <**/*.byte>: ocaml, byte, program\n\
     <**/*.odoc>: ocaml, doc\n\
     <**/*.native>: ocaml, native, program\n\
     <**/*.cma>: ocaml, byte, library\n\
     <**/*.cmxa>: ocaml, native, library\n\
     <**/*.cmo>: ocaml, byte\n\
     <**/*.cmi>: ocaml, byte, native\n\
     <**/*.cmx>: ocaml, native\n\
     <**/*.mly>: infer\n\
     <**/.svn>|\".bzr\"|\".hg\"|\".git\"|\"_darcs\": -traverse\n\
    ";

  List.iter
    (Configuration.parse_string ~source:Const.Source.command_line)
    !Options.tag_lines;

  Configuration.tag_any !Options.tags;
  if !Options.recursive || Options.ocamlbuild_project_heuristic ()
  then Configuration.tag_any ["traverse"];

  (* options related to findlib *)
  if !Options.use_ocamlfind then
    List.iter
      (fun pkg ->
        let tag = Param_tags.make "package" pkg in
        Configuration.tag_any [tag])
      !Options.ocaml_pkgs;

  begin match !Options.ocaml_syntax with
  | Some syntax -> Configuration.tag_any [Param_tags.make "syntax" syntax]
  | None -> () end;

  let newpwd = Sys.getcwd () in
  Sys.chdir Pathname.pwd;
  let entry_include_dirs = ref [] in
  let entry =
    Slurp.filter
      begin fun path name () ->
        let dir =
          if path = Filename.current_dir_name then
            None
          else
            Some path
        in
        let path_name = path/name in

        if name = "_tags" then begin
          let tags_path =
            (* PR#6482: remember that this code is run lazily by the Slurp command,
               and may run only after the working directory has been changed.

               On the other hand, always using the absolute path makes
               error messages longer and more frigthening in case of
               syntax error in the _tags file. So we use the absolute
               path only when necessary -- the working directory has
               changed. *)
            if Sys.getcwd () = Pathname.pwd then path_name
            else Pathname.pwd / path_name in
          ignore (Configuration.parse_file ?dir tags_path);
        end;

        (List.mem name ["_oasis"] || (String.length name > 0 && name.[0] <> '_'))
        && (name <> !Options.build_dir && not (List.mem name !Options.exclude_dirs))
        && begin
          not (path_name <> Filename.current_dir_name && Pathname.is_directory path_name)
          || begin
            let tags = tags_of_pathname path_name in
            (if Tags.mem "include" tags
              || List.mem path_name !Options.include_dirs then
              (entry_include_dirs := path_name :: !entry_include_dirs; true)
            else
              Tags.mem "traverse" tags
              || List.exists (Pathname.is_prefix path_name) !Options.include_dirs
              || List.exists (Pathname.is_prefix path_name) target_dirs)
            && ((* beware: !Options.build_dir is an absolute directory *)
                Pathname.normalize !Options.build_dir
                <> Pathname.normalize (Pathname.pwd/path_name))
          end
        end
      end
      (Slurp.slurp Filename.current_dir_name)
  in
  Hooks.call_hook Hooks.Before_hygiene;
  let hygiene_entry =
    Slurp.map begin fun path name () ->
      let tags = tags_of_pathname (path/name) in
      not (Tags.mem "not_hygienic" tags) && not (Tags.mem "precious" tags)
    end entry in
  Slurp.force hygiene_entry;
  if !Options.hygiene && not first_run_for_plugin then
    Fda.inspect hygiene_entry;
  let entry = hygiene_entry in
  Hooks.call_hook Hooks.After_hygiene;
  Options.include_dirs := Pathname.current_dir_name :: List.rev !entry_include_dirs;
  dprintf 3 "include directories are:@ %a" print_string_list !Options.include_dirs;
  Options.entry := Some entry;

  Hooks.call_hook Hooks.Before_rules;
  Ocaml_specific.init ();
  Hooks.call_hook Hooks.After_rules;

  Sys.chdir options_wd;
  Plugin.execute_plugin_if_needed ();

  (* [Param_tags.init ()] is called *after* the plugin is executed, as
     some of the parametrized tags present in the _tags files parsed
     will be declared by the plugin, and would therefore result in
     "tag X does not expect a parameter" warnings if initialized
     before. Note that [Plugin.rebuild_plugin_if_needed] is careful to
     partially initialize the tags that it uses for plugin compilation. *)
  Param_tags.init ();

  Sys.chdir newpwd;
  (*let () = dprintf 0 "source_dir_path_set:@ %a" StringSet.print source_dir_path_set*)

  if !Options.show_documentation then begin
    show_documentation ();
    raise Exit_silently
  end;

  let all_tags =
    let builtin = builtin_useful_tags in
    let used_in_flags = Flags.get_used_tags () in
    let used_in_deps =
      List.fold_left (fun acc (tags, _deps) -> Tags.union acc tags)
        Tags.empty (Command.list_all_deps ())
    in
    Tags.union builtin (Tags.union used_in_flags used_in_deps) in
  Configuration.check_tags_usage all_tags;

  Digest_cache.init ();

  Sys.catch_break true;

  show_tags ();

  let targets =
    List.map begin fun starget ->
      let starget = Resource.import starget in
      let target = path_and_context_of_string starget in
      let ext = Pathname.get_extension starget in
      (target, starget, ext)
    end !Options.targets in

  try
    let targets =
      List.map begin fun (target, starget, ext) ->
        Shell.mkdir_p (Pathname.dirname starget);
        let target = Solver.solve_target starget target in
        (target, ext)
      end targets in

    Command.dump_parallel_stats ();

    Log.finish ();

    Shell.chdir Pathname.pwd;

    let call spec = sys_command (Command.string_of_command_spec spec) in

    let cmds =
      List.fold_right begin fun (target, ext) acc ->
        let cmd = !Options.build_dir/target in
        let link x =
          if !Options.make_links then ignore (call (S [A"ln"; A"-sf"; P x; A Pathname.current_dir_name])) in
        match ext with
        | "byte" | "native" | "top" ->
            link cmd; cmd :: acc
        | "html" ->
            link (Pathname.dirname cmd); acc
        | _ ->
            if !Options.program_to_execute then
              eprintf "Warning: Won't execute %s whose extension is neither .byte nor .native" cmd;
            acc
      end targets [] in

    if !Options.program_to_execute then
      begin
        match List.rev cmds with
        | [] -> raise (Exit_usage "Using -- requires one target");
        | cmd :: rest ->
          if rest <> [] then dprintf 0 "Warning: Using -- only run the last target";
          let cmd_spec = S [P cmd; atomize !Options.program_args] in
          dprintf 3 "Running the user command:@ %a" Pathname.print cmd;
          raise (Exit_with_code (call cmd_spec)) (* Exit with the exit code of the called command *)
      end
    else
      ()
  with
  | Ocaml_dependencies.Circular_dependencies(cycle, p) ->
      raise
        (Exit_build_error
          (sbprintf "@[<2>Circular dependencies: %S already seen in@ %a@]@." p pp_l cycle))
;;

open Exit_codes;;

let main () =
  let exit rc =
    Log.finish ~how:(if rc <> 0 then `Error else `Success) ();
    Pervasives.exit rc
  in
  try
    proceed ()
  with e ->
    if !Options.catch_errors then
      try raise e with
      | Exit_OK -> exit rc_ok
      | Fda.Exit_hygiene_failed ->
          Log.eprintf "Exiting due to hygiene violations.";
          exit rc_hygiene
      | Exit_usage u ->
          Log.eprintf "Usage:@ %s." u;
          exit rc_usage
      | Exit_system_error msg ->
          Log.eprintf "System error:@ %s." msg;
          exit rc_system_error
      | Exit_with_code rc ->
          exit rc
      | Exit_silently ->
          Log.finish ~how:`Quiet ();
          Pervasives.exit rc_ok
      | Exit_silently_with_code rc ->
          Log.finish ~how:`Quiet ();
          Pervasives.exit rc
      | Solver.Failed backtrace ->
          Log.raw_dprintf (-1) "@[<v0>@[<2>Solver failed:@ %a@]@."
            Report.print_backtrace_analyze backtrace;
          Log.raw_dprintf 1 "@[<v2>Backtrace:%a@]@]@."
            Report.print_backtrace backtrace;
          exit rc_solver_failed
      | Failure s ->
          Log.eprintf "Failure:@ %s." s;
          exit rc_failure
      | Solver.Circular(r, rs) ->
          Log.eprintf "Circular build detected@ (%a already seen in %a)"
          Resource.print r (List.print Resource.print) rs;
          exit rc_circularity
      | Invalid_argument s ->
          Log.eprintf
            "INTERNAL ERROR: Invalid argument %s\n\
            This is likely to be a bug, please report this to the ocamlbuild\n\
            developers." s;
          exit rc_invalid_argument
      | Ocaml_utils.Ocamldep_error msg ->
          Log.eprintf "Ocamldep error: %s" msg;
          exit rc_ocamldep_error
      | Lexers.Error (msg,loc) ->
          Log.eprintf "%aLexing error: %s." Loc.print_loc loc msg;
          exit rc_lexing_error
      | Arg.Bad msg ->
          Log.eprintf "%s" msg;
          exit rc_usage
      | Exit_build_error msg ->
          Log.eprintf "%s" msg;
          exit rc_build_error
      | Arg.Help msg ->
          Log.eprintf "%s" msg;
          exit rc_ok
      | e ->
          try
            Log.eprintf "%a" My_unix.report_error e;
            exit 100
          with
          | e ->
            Log.eprintf "Exception@ %s." (Printexc.to_string e);
            exit 100
    else raise e
;;
