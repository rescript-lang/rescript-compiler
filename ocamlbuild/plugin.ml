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


(* Original author: Nicolas Pouillard *)
open My_std
open Format
open Log
open Pathname.Operators
open Tags.Operators
open Rule
open Tools
open Command
;;


let plugin                = "myocamlbuild"
let plugin_file           = plugin^".ml"
let plugin_config_file    = plugin^"_config.ml"
let plugin_config_file_interface = plugin^"_config.mli"
let we_need_a_plugin ()      = !Options.plugin && sys_file_exists plugin_file
let we_have_a_plugin ()      = sys_file_exists ((!Options.build_dir/plugin)^(!Options.exe))
let we_have_a_config_file () = sys_file_exists plugin_config_file
let we_have_a_config_file_interface () = sys_file_exists plugin_config_file_interface

module Make(U:sig end) =
  struct
    let we_need_a_plugin = we_need_a_plugin ()
    let we_have_a_plugin = we_have_a_plugin ()
    let we_have_a_config_file = we_have_a_config_file ()
    let we_have_a_config_file_interface = we_have_a_config_file_interface ()
    let up_to_date_or_copy fn =
      let fn' = !Options.build_dir/fn in
      Pathname.exists fn &&
        begin
          Pathname.exists fn' && Pathname.same_contents fn fn' ||
          begin
            Shell.cp fn fn';
            false
          end
        end

    let rebuild_plugin_if_needed () =
      let a = up_to_date_or_copy plugin_file in
      let b = (not we_have_a_config_file) || up_to_date_or_copy plugin_config_file in
      let c = (not we_have_a_config_file_interface) || up_to_date_or_copy plugin_config_file_interface in
      if a && b && c && we_have_a_plugin then
        () (* Up to date *)
           (* FIXME: remove ocamlbuild_config.ml in _build/ if removed in parent *)
      else begin
        if !Options.native_plugin
            && not (sys_file_exists ((!Ocamlbuild_where.libdir)/"ocamlbuildlib.cmxa")) then
          begin
            Options.native_plugin := false;
            eprintf "Warning: Won't be able to compile a native plugin"
          end;
        let plugin_config =
          if we_have_a_config_file then
            if we_have_a_config_file_interface then
              S[P plugin_config_file_interface; P plugin_config_file]
            else P plugin_config_file
          else N in

        let cma, cmo, compiler, byte_or_native =
          if !Options.native_plugin then
            "cmxa", "cmx", !Options.ocamlopt, "native"
          else
            "cma", "cmo", !Options.ocamlc, "byte"
        in


        let (unix_spec, ocamlbuild_lib_spec, ocamlbuild_module_spec) =

          let use_light_mode =
            not !Options.native_plugin && !*My_unix.is_degraded in
          let use_ocamlfind_pkgs =
            !Options.use_ocamlfind && !Options.plugin_tags <> [] in
          (* The plugin has the following dependencies that must be
             included during compilation:

             - unix.cmxa, if it is available
             - ocamlbuildlib.cm{a,xa}, the library part of ocamlbuild
             - ocamlbuild.cm{o,x}, the module that performs the
               initialization work of the ocamlbuild executable, using
               modules of ocamlbuildlib.cmxa

             We pass all this stuff to the compilation command for the
             plugin, with two independent important details to handle:

             (1) ocamlbuild is designed to still work in environments
             where Unix is not available for some reason; in this
             case, we should not link unix, and use the
             "ocamlbuildlight.cmo" initialization module, which runs
             a "light" version of ocamlbuild without unix. There is
             also an ocamlbuildlightlib.cma archive to be used in that
             case.

             The boolean variable [use_light_mode] tells us whether we
             are in this unix-deprived scenario.

             (2) there are risks of compilation error due to
             double-linking of native modules when the user passes its
             own tags to the plugin compilation process (as was added
             to support modular construction of
             ocamlbuild plugins). Indeed, if we hard-code linking to
             unix.cmxa in all cases, and the user
             enables -use-ocamlfind and
             passes -plugin-tag "package(unix)" (or package(foo) for
             any foo which depends on unix), the command-line finally
             executed will be

               ocamlfind ocamlopt unix.cmxa -package unix myocamlbuild.ml

             which fails with a compilation error due to doubly-passed
             native modules.

             To sanest way to solve this problem at the ocamlbuild level
             is to pass "-package unix" instead of unix.cmxa when we
             detect that such a situation may happen. OCamlfind will see
             that the same package is demanded twice, and only request
             it once to the compiler. Similarly, we use "-package
             ocamlbuild" instead of linking ocamlbuildlib.cmxa[1].

             We switch to this behavior when two conditions, embodied in
             the boolean variable [use_ocamlfind_pkgs], are met:
             (a) use-ocamlfind is enabled
             (b) the user is passing some plugin tags

             Condition (a) is overly conservative as the double-linking
             issue may also happen in non-ocamlfind situations, such as
             "-plugin-tags use_unix" -- but it's unclear how one would
             avoid the issue in that case, except by documenting that
             people should not do that, or getting rid of the
             hard-linking logic entirely, with the corresponding risks
             of regression.

             Condition (b) should not be necessary (we expect using
             ocamlfind packages to work whenever ocamlfind
             is available), but allows the behavior in absence
             of -plugin-tags to be completely unchanged, to reassure us
             about potential regressions introduced by this option.

             [1]: we may wonder whether to use "-package ocamlbuildlight"
             in unix-deprived situations, but currently ocamlfind
             doesn't know about the ocamlbuildlight library. As
             a compromise we always use "-package ocamlbuild" when
             use_ocamlfind_pkgs is set. An ocamlfind and -plugin-tags
             user in unix-deprived environment may want to mutate the
             META of ocamlbuild to point to ocamlbuildlightlib instead
             of ocamlbuildlib.
          *)

          let unix_lib =
            if use_ocamlfind_pkgs then `Package "unix"
            else if use_light_mode then `Nothing
            else `Lib "unix" in

          let ocamlbuild_lib =
            if use_ocamlfind_pkgs then `Package "ocamlbuild"
            else if use_light_mode then `Local_lib "ocamlbuildlightlib"
            else `Local_lib "ocamlbuildlib" in

          let ocamlbuild_module =
            if use_light_mode then `Local_mod "ocamlbuildlight"
            else `Local_mod "ocamlbuild" in

          let dir = !Ocamlbuild_where.libdir in
          let dir = if Pathname.is_implicit dir then Pathname.pwd/dir else dir in

          let in_dir file =
            let path = dir/file in
            if not (sys_file_exists path) then failwith
              (sprintf "Cannot find %S in ocamlbuild -where directory" file);
            path in

          let spec = function
            | `Nothing -> N
            | `Package pkg -> S[A "-package"; A pkg]
            | `Lib lib -> P (lib -.- cma)
            | `Local_lib llib -> S [A "-I"; A dir; P (in_dir (llib -.- cma))]
            | `Local_mod lmod -> P (in_dir (lmod -.- cmo)) in

          (spec unix_lib, spec ocamlbuild_lib, spec ocamlbuild_module)
        in

        let plugin_tags =
          Tags.of_list !Options.plugin_tags
          ++ "ocaml" ++ "program" ++ "link" ++ byte_or_native in

        (* The plugin is compiled before [Param_tags.init()] is called
           globally, which means that parametrized tags have not been
           made effective yet. The [partial_init] calls below initializes
           precisely those that will be used during the compilation of
           the plugin, and no more.
        *)
        Param_tags.partial_init Const.Source.plugin_tag plugin_tags;

        let cmd =
          (* The argument order is important: we carefully put the
             plugin source files before the ocamlbuild.cm{o,x} module
             doing the main initialization, so that user global
             side-effects (setting options, installing flags..) are
             performed brefore ocamlbuild's main routine. This is
             a fragile thing to rely upon and we insist that our users
             use the more robust [dispatch] registration instead, but
             we still aren't going to break that now.

             For the same reason we place the user plugin-tags after
             the plugin libraries (in case a tag would, say, inject
             a .cmo that also relies on them), but before the main
             plugin source file and ocamlbuild's initialization. *)
          Cmd(S[compiler;
                unix_spec; ocamlbuild_lib_spec;
                T plugin_tags;
                plugin_config; P plugin_file;
                ocamlbuild_module_spec;
                A"-o"; Px (plugin^(!Options.exe))])
        in
        Shell.chdir !Options.build_dir;
        Shell.rm_f (plugin^(!Options.exe));
        Command.execute cmd;
        if !Options.just_plugin then begin
          Log.finish ();
          raise Exit_OK;
        end;
      end

    let execute_plugin_if_needed () =
      if we_need_a_plugin then
        begin
          rebuild_plugin_if_needed ();
          Shell.chdir Pathname.pwd;
          let runner = if !Options.native_plugin then N else !Options.ocamlrun in
          let argv = List.tl (Array.to_list Sys.argv) in
          let passed_argv = List.filter (fun s -> s <> "-plugin-option") argv in
          let spec = S[runner; P(!Options.build_dir/plugin^(!Options.exe));
                       A"-no-plugin"; atomize passed_argv] in
          Log.finish ();
          let rc = sys_command (Command.string_of_command_spec spec) in
          raise (Exit_silently_with_code rc);
        end
      else if not (sys_file_exists plugin_file) && !Options.plugin_tags <> [] then
        eprintf "Warning: option -plugin-tag(s) has no effect \
                 in absence of plugin file %S" plugin_file
      else
        ()
  end
;;

let execute_plugin_if_needed () =
  let module P = Make(struct end) in
  P.execute_plugin_if_needed ()
;;
