(***********************************************************************)
(*                                                                     *)
(*                             OCamldoc                                *)
(*                                                                     *)
(*            Maxence Guesdon, projet Cristal, INRIA Rocquencourt      *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(** Command-line arguments. *)

module M = Odoc_messages

let current_generator = ref (None : Odoc_gen.generator option)

let get_html_generator () =
  match !current_generator with
    None -> (module Odoc_html.Generator : Odoc_html.Html_generator)
  | Some (Odoc_gen.Html m) -> m
  | Some _ -> failwith (M.current_generator_is_not "html")
;;

let get_latex_generator () =
  match !current_generator with
    None -> (module Odoc_latex.Generator : Odoc_latex.Latex_generator)
  | Some (Odoc_gen.Latex m) -> m
  | Some _ -> failwith (M.current_generator_is_not "latex")
;;

let get_texi_generator () =
  match !current_generator with
    None -> (module Odoc_texi.Generator : Odoc_texi.Texi_generator)
  | Some (Odoc_gen.Texi m) -> m
  | Some _ -> failwith (M.current_generator_is_not "texi")
;;

let get_man_generator () =
  match !current_generator with
    None -> (module Odoc_man.Generator : Odoc_man.Man_generator)
  | Some (Odoc_gen.Man m) -> m
  | Some _ -> failwith (M.current_generator_is_not "man")
;;

let get_dot_generator () =
  match !current_generator with
    None -> (module Odoc_dot.Generator : Odoc_dot.Dot_generator)
  | Some (Odoc_gen.Dot m) -> m
  | Some _ -> failwith (M.current_generator_is_not "dot")
;;

let get_base_generator () =
  match !current_generator with
    None -> (module Odoc_gen.Base_generator : Odoc_gen.Base)
  | Some (Odoc_gen.Base m) -> m
  | Some _ -> failwith (M.current_generator_is_not "base")
;;

let extend_html_generator f =
  let current = get_html_generator () in
  let module Current = (val current : Odoc_html.Html_generator) in
  let module F = (val f : Odoc_gen.Html_functor) in
  let module M = F(Current) in
  current_generator := Some (Odoc_gen.Html (module M : Odoc_html.Html_generator))
;;

let extend_latex_generator f =
  let current = get_latex_generator () in
  let module Current = (val current : Odoc_latex.Latex_generator) in
  let module F = (val f : Odoc_gen.Latex_functor) in
  let module M = F(Current) in
  current_generator := Some(Odoc_gen.Latex (module M : Odoc_latex.Latex_generator))
;;

let extend_texi_generator f =
  let current = get_texi_generator () in
  let module Current = (val current : Odoc_texi.Texi_generator) in
  let module F = (val f : Odoc_gen.Texi_functor) in
  let module M = F(Current) in
  current_generator := Some(Odoc_gen.Texi (module M : Odoc_texi.Texi_generator))
;;

let extend_man_generator f =
  let current = get_man_generator () in
  let module Current = (val current : Odoc_man.Man_generator) in
  let module F = (val f : Odoc_gen.Man_functor) in
  let module M = F(Current) in
  current_generator := Some(Odoc_gen.Man (module M : Odoc_man.Man_generator))
;;

let extend_dot_generator f =
  let current = get_dot_generator () in
  let module Current = (val current : Odoc_dot.Dot_generator) in
  let module F = (val f : Odoc_gen.Dot_functor) in
  let module M = F(Current) in
  current_generator := Some (Odoc_gen.Dot (module M : Odoc_dot.Dot_generator))
;;

let extend_base_generator f =
  let current = get_base_generator () in
  let module Current = (val current : Odoc_gen.Base) in
  let module F = (val f : Odoc_gen.Base_functor) in
  let module M = F(Current) in
  current_generator := Some (Odoc_gen.Base (module M : Odoc_gen.Base))
;;

(** Analysis of a string defining options. Return the list of
   options according to the list giving associations between
   [(character, _)] and a list of options. *)
let analyse_option_string l s =
  List.fold_left
    (fun acc -> fun ((c,_), v) ->
      if String.contains s c then
        acc @ v
      else
        acc)
    []
    l

(** Analysis of a string defining the merge options to be used.
   Returns the list of options specified.*)
let analyse_merge_options s =
  let l = [
    (M.merge_description, [Odoc_types.Merge_description]) ;
    (M.merge_author, [Odoc_types.Merge_author]) ;
    (M.merge_version, [Odoc_types.Merge_version]) ;
    (M.merge_see, [Odoc_types.Merge_see]) ;
    (M.merge_since, [Odoc_types.Merge_since]) ;
    (M.merge_before, [Odoc_types.Merge_before]) ;
    (M.merge_deprecated, [Odoc_types.Merge_deprecated]) ;
    (M.merge_param, [Odoc_types.Merge_param]) ;
    (M.merge_raised_exception, [Odoc_types.Merge_raised_exception]) ;
    (M.merge_return_value, [Odoc_types.Merge_return_value]) ;
    (M.merge_custom, [Odoc_types.Merge_custom]) ;
    (M.merge_all, Odoc_types.all_merge_options)
  ]
  in
  analyse_option_string l s


let f_latex_title s =
  try
    let pos = String.index s ',' in
    let n = int_of_string (String.sub s 0 pos) in
    let len = String.length s in
    let command = String.sub s (pos + 1) (len - pos - 1) in
    Odoc_latex.latex_titles := List.remove_assoc n !Odoc_latex.latex_titles ;
    Odoc_latex.latex_titles := (n, command) :: !Odoc_latex.latex_titles
  with
    Not_found
  | Invalid_argument _ ->
      incr Odoc_global.errors ;
      prerr_endline (M.wrong_format s)

let add_hidden_modules s =
  let l = Str.split (Str.regexp ",") s in
  List.iter
    (fun n ->
      let name = Str.global_replace (Str.regexp "[ \n\r\t]+") "" n in
      match name with
        "" -> ()
      | _ ->
          match name.[0] with
            'A'..'Z' -> Odoc_global.hidden_modules := name :: !Odoc_global.hidden_modules
          | _ ->
              incr Odoc_global.errors;
              prerr_endline (M.not_a_module_name name)
    )
    l

let set_generator (g : Odoc_gen.generator) = current_generator := Some g

let anonymous f =
  let sf =
    if Filename.check_suffix f "ml" then
      Odoc_global.Impl_file f
    else
        if Filename.check_suffix f !Config.interface_suffix then
        Odoc_global.Intf_file f
      else
        if Filename.check_suffix f "txt" then
          Odoc_global.Text_file f
        else
          failwith (Odoc_messages.unknown_extension f)
  in
  Odoc_global.files := !Odoc_global.files @ [sf]

module Options = Main_args.Make_ocamldoc_options(struct
  let set r () = r := true
  let unset r () = r := false
  let _absname = set Location.absname
  let _I s = Odoc_global.include_dirs :=
       (Misc.expand_directory Config.standard_library s) :: !Odoc_global.include_dirs
  let _impl s = Odoc_global.files := !Odoc_global.files @ [Odoc_global.Impl_file s]
  let _intf s = Odoc_global.files := !Odoc_global.files @ [Odoc_global.Intf_file s]
  let _intf_suffix s = Config.interface_suffix := s
  let _labels = unset Clflags.classic
  let _no_alias_deps = set Clflags.transparent_modules
  let _no_app_funct = unset Clflags.applicative_functors
  let _noassert = set Clflags.noassert
  let _nolabels = set Clflags.classic
  let _nostdlib = set Clflags.no_std_include
  let _open s = Clflags.open_modules := s :: !Clflags.open_modules
  let _pp s = Clflags.preprocessor := Some s
  let _ppx s = Clflags.all_ppx := s :: !Clflags.all_ppx
  let _principal = set Clflags.principal
  let _rectypes = set Clflags.recursive_types
  let _safe_string = unset Clflags.unsafe_string
  let _short_paths = unset Clflags.real_paths
  let _strict_sequence = set Clflags.strict_sequence
  let _strict_formats = set Clflags.strict_formats
  let _thread = set Clflags.use_threads
  let _vmthread = set Clflags.use_vmthreads
  let _unsafe () = assert false
  let _unsafe_string = set Clflags.unsafe_string
  let _v () = Compenv.print_version_and_library "documentation generator"
  let _version = Compenv.print_version_string
  let _vnum = Compenv.print_version_string
  let _w = (Warnings.parse_options false)
  let _warn_error _ = assert false
  let _warn_help _ = assert false
  let _where = Compenv.print_standard_library
  let _verbose = set Clflags.verbose
  let _nopervasives = set Clflags.nopervasives
  let _dsource = set Clflags.dump_source
  let _dparsetree = set Clflags.dump_parsetree
  let _dtypedtree = set Clflags.dump_typedtree
  let _drawlambda = set Clflags.dump_rawlambda
  let _dlambda = set Clflags.dump_lambda
  let _dinstr = set Clflags.dump_instr
  let anonymous = anonymous
end)

(** The default option list *)
let default_options = Options.list @
[
  "-text", Arg.String (fun s ->
       Odoc_global.files := !Odoc_global.files @ [Odoc_global.Text_file s]),
    M.option_text ;
  "-warn-error", Arg.Set Odoc_global.warn_error, M.werr ;
  "-hide-warnings", Arg.Clear Odoc_config.print_warnings, M.hide_warnings ;
  "-o", Arg.String (fun s -> Odoc_global.out_file := s), M.out_file ;
  "-d", Arg.String (fun s -> Odoc_global.target_dir := s), M.target_dir ;
  "-sort", Arg.Unit (fun () -> Odoc_global.sort_modules := true), M.sort_modules ;
  "-no-stop", Arg.Set Odoc_global.no_stop, M.no_stop ;
  "-no-custom-tags", Arg.Set Odoc_global.no_custom_tags, M.no_custom_tags ;
  "-stars", Arg.Set Odoc_global.remove_stars, M.remove_stars ;
  "-inv-merge-ml-mli", Arg.Set Odoc_global.inverse_merge_ml_mli, M.inverse_merge_ml_mli ;
  "-no-module-constraint-filter", Arg.Clear Odoc_global.filter_with_module_constraints,
  M.no_filter_with_module_constraints ;

  "-keep-code", Arg.Set Odoc_global.keep_code, M.keep_code^"\n" ;

  "-dump", Arg.String (fun s -> Odoc_global.dump := Some s), M.dump ;
  "-load", Arg.String (fun s -> Odoc_global.load := !Odoc_global.load @ [s]), M.load^"\n" ;

  "-t", Arg.String (fun s -> Odoc_global.title := Some s), M.option_title ;
  "-intro", Arg.String (fun s -> Odoc_global.intro_file := Some s), M.option_intro ;
  "-hide", Arg.String add_hidden_modules, M.hide_modules ;
  "-m", Arg.String (fun s -> Odoc_global.merge_options := !Odoc_global.merge_options @ (analyse_merge_options s)),
  M.merge_options ^
  "\n\n *** choosing a generator ***\n";

(* generators *)
  "-html", Arg.Unit (fun () -> set_generator
       (Odoc_gen.Html (module Odoc_html.Generator : Odoc_html.Html_generator))),
    M.generate_html ;
  "-latex", Arg.Unit (fun () -> set_generator
       (Odoc_gen.Latex (module Odoc_latex.Generator : Odoc_latex.Latex_generator))),
    M.generate_latex ;
  "-texi", Arg.Unit (fun () -> set_generator
       (Odoc_gen.Texi (module Odoc_texi.Generator : Odoc_texi.Texi_generator))),
    M.generate_texinfo ;
  "-man", Arg.Unit (fun () -> set_generator
       (Odoc_gen.Man (module Odoc_man.Generator : Odoc_man.Man_generator))),
    M.generate_man ;
  "-dot", Arg.Unit (fun () -> set_generator
       (Odoc_gen.Dot (module Odoc_dot.Generator : Odoc_dot.Dot_generator))),
    M.generate_dot ;
  "-customdir", Arg.Unit (fun () -> Printf.printf "%s\n" Odoc_config.custom_generators_path; exit 0),
  M.display_custom_generators_dir ;
  "-i", Arg.String (fun s -> ()), M.add_load_dir ;
  "-g", Arg.String (fun s -> ()), M.load_file ^
  "\n\n *** HTML options ***\n";

(* html only options *)
  "-all-params", Arg.Set Odoc_html.with_parameter_list, M.with_parameter_list ;
  "-css-style", Arg.String (fun s -> Odoc_html.css_style := Some s), M.css_style ;
  "-index-only", Arg.Set Odoc_html.index_only, M.index_only ;
  "-colorize-code", Arg.Set Odoc_html.colorize_code, M.colorize_code ;
  "-short-functors", Arg.Set Odoc_html.html_short_functors, M.html_short_functors ;
  "-charset", Arg.Set_string Odoc_html.charset, (M.charset !Odoc_html.charset)^
  "\n\n *** LaTeX options ***\n";

(* latex only options *)
  "-noheader", Arg.Unit (fun () -> Odoc_global.with_header := false), M.no_header ;
  "-notrailer", Arg.Unit (fun () -> Odoc_global.with_trailer := false), M.no_trailer ;
  "-sepfiles", Arg.Set Odoc_latex.separate_files, M.separate_files ;
  "-latextitle", Arg.String f_latex_title, M.latex_title Odoc_latex.latex_titles ;
  "-latex-value-prefix",
    Arg.String (fun s -> Odoc_latex.latex_value_prefix := s), M.latex_value_prefix ;
  "-latex-type-prefix",
    Arg.String (fun s -> Odoc_latex.latex_type_prefix := s), M.latex_type_prefix ;
  "-latex-exception-prefix",
    Arg.String (fun s -> Odoc_latex.latex_exception_prefix := s), M.latex_exception_prefix ;
  "-latex-attribute-prefix",
    Arg.String (fun s -> Odoc_latex.latex_attribute_prefix := s), M.latex_attribute_prefix ;
  "-latex-method-prefix",
    Arg.String (fun s -> Odoc_latex.latex_method_prefix := s), M.latex_method_prefix ;
  "-latex-module-prefix",
    Arg.String (fun s -> Odoc_latex.latex_module_prefix := s), M.latex_module_prefix ;
  "-latex-module-type-prefix",
    Arg.String (fun s -> Odoc_latex.latex_module_type_prefix := s), M.latex_module_type_prefix ;
  "-latex-class-prefix",
    Arg.String (fun s -> Odoc_latex.latex_class_prefix := s), M.latex_class_prefix ;
  "-latex-class-type-prefix",
    Arg.String (fun s -> Odoc_latex.latex_class_type_prefix := s), M.latex_class_type_prefix ;
  "-notoc", Arg.Unit (fun () -> Odoc_global.with_toc := false), M.no_toc ^
  "\n\n *** texinfo options ***\n";

(* texi only options *)
  "-noindex", Arg.Clear Odoc_global.with_index, M.no_index ;
  "-esc8", Arg.Set Odoc_texi.esc_8bits, M.esc_8bits ;
  "-info-section", Arg.String ((:=) Odoc_texi.info_section), M.info_section ;
  "-info-entry", Arg.String (fun s -> Odoc_texi.info_entry := !Odoc_texi.info_entry @ [ s ]),
  M.info_entry ^
  "\n\n *** dot options ***\n";

(* dot only options *)
  "-dot-colors", Arg.String (fun s -> Odoc_dot.dot_colors := Str.split (Str.regexp_string ",") s), M.dot_colors ;
  "-dot-include-all", Arg.Set Odoc_dot.dot_include_all, M.dot_include_all ;
  "-dot-types", Arg.Set Odoc_dot.dot_types, M.dot_types ;
  "-dot-reduce", Arg.Set Odoc_dot.dot_reduce, M.dot_reduce^
  "\n\n *** man pages options ***\n";

(* man only options *)
  "-man-mini", Arg.Set Odoc_man.man_mini, M.man_mini ;
  "-man-suffix", Arg.String (fun s -> Odoc_man.man_suffix := s), M.man_suffix ;
  "-man-section", Arg.String (fun s -> Odoc_man.man_section := s), M.man_section ;

]

let options = ref default_options

let modified_options () =
  !options != default_options

let append_last_doc suffix =
  match List.rev !options with
  | (key, spec, doc) :: tl ->
      options := List.rev ((key, spec, doc ^ suffix) :: tl)
  | [] -> ()

(** The help option list, overriding the default ones from the Arg module *)
let help_options = ref []
let help_action () =
  let msg =
    Arg.usage_string
      (!options @ !help_options)
      (M.usage ^ M.options_are) in
  print_string msg
let () =
  help_options := [
    "-help", Arg.Unit help_action, M.help ;
    "--help", Arg.Unit help_action, M.help
]

let add_option o =
  if not (modified_options ()) then
    append_last_doc "\n *** custom generator options ***\n";
  let (s,_,_) = o in
  let rec iter = function
      [] -> [o]
    | (s2,f,m) :: q ->
        if s = s2 then
          o :: q
        else
          (s2,f,m) :: (iter q)
  in
  options := iter !options

let parse () =
  if modified_options () then append_last_doc "\n";
  let options = !options @ !help_options in
  let _ = Arg.parse (Arg.align ~limit:13 options)
      anonymous
      (M.usage^M.options_are)
  in
  (* we sort the hidden modules by name, to be sure that for example,
     A.B is before A, so we will match against A.B before A in
     Odoc_name.hide_modules.*)
  Odoc_global.hidden_modules :=
    List.sort (fun a -> fun b -> - (compare a b)) !Odoc_global.hidden_modules
