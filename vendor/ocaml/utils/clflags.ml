(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Command-line parameters *)

let objfiles = ref ([] : string list)   (* .cmo and .cma files *)
and ccobjs = ref ([] : string list)     (* .o, .a, .so and -cclib -lxxx *)
and dllibs = ref ([] : string list)     (* .so and -dllib -lxxx *)

let compile_only = ref false            (* -c *)
and output_name = ref (None : string option) (* -o *)
and include_dirs = ref ([] : string list)(* -I *)
and no_std_include = ref false          (* -nostdlib *)
and print_types = ref false             (* -i *)
and make_archive = ref false            (* -a *)
and debug = ref false                   (* -g *)
and fast = ref false                    (* -unsafe *)
and link_everything = ref false         (* -linkall *)
and custom_runtime = ref false          (* -custom *)
and no_check_prims = ref false          (* -no-check-prims *)
and bytecode_compatible_32 = ref false  (* -compat-32 *)
and output_c_object = ref false         (* -output-obj *)
and output_complete_object = ref false  (* -output-complete-obj *)
and all_ccopts = ref ([] : string list)     (* -ccopt *)
and classic = ref false                 (* -nolabels *)
and nopervasives = ref false            (* -nopervasives *)
and preprocessor = ref(None : string option) (* -pp *)
and all_ppx = ref ([] : string list)        (* -ppx *)
let annotations = ref false             (* -annot *)
let binary_annotations = ref false      (* -annot *)
and use_threads = ref false             (* -thread *)
and use_vmthreads = ref false           (* -vmthread *)
and noassert = ref false                (* -noassert *)
and verbose = ref false                 (* -verbose *)
and noprompt = ref false                (* -noprompt *)
and nopromptcont = ref false            (* -nopromptcont *)
and init_file = ref (None : string option)   (* -init *)
and noinit = ref false                  (* -noinit *)
and open_modules = ref []               (* -open *)
and use_prims = ref ""                  (* -use-prims ... *)
and use_runtime = ref ""                (* -use-runtime ... *)
and principal = ref false               (* -principal *)
and real_paths = ref true               (* -short-paths *)
and recursive_types = ref false         (* -rectypes *)
and strict_sequence = ref false         (* -strict-sequence *)
and strict_formats = ref false          (* -strict-formats *)
and applicative_functors = ref true     (* -no-app-funct *)
and make_runtime = ref false            (* -make-runtime *)
and gprofile = ref false                (* -p *)
and c_compiler = ref (None: string option) (* -cc *)
and no_auto_link = ref false            (* -noautolink *)
and dllpaths = ref ([] : string list)   (* -dllpath *)
and make_package = ref false            (* -pack *)
and for_package = ref (None: string option) (* -for-pack *)
and error_size = ref 500                (* -error-size *)
and float_const_prop = ref true         (* -no-float-const-prop *)
and transparent_modules = ref false     (* -trans-mod *)
let dump_source = ref false             (* -dsource *)
let dump_parsetree = ref false          (* -dparsetree *)
and dump_typedtree = ref false          (* -dtypedtree *)
and dump_rawlambda = ref false          (* -drawlambda *)
and dump_lambda = ref false             (* -dlambda *)
and dump_clambda = ref false            (* -dclambda *)
and dump_instr = ref false              (* -dinstr *)

let keep_asm_file = ref false           (* -S *)
let optimize_for_speed = ref true       (* -compact *)
and opaque = ref false                  (* -opaque *)

and dump_cmm = ref false                (* -dcmm *)
let dump_selection = ref false          (* -dsel *)
let dump_cse = ref false                (* -dcse *)
let dump_live = ref false               (* -dlive *)
let dump_spill = ref false              (* -dspill *)
let dump_split = ref false              (* -dsplit *)
let dump_interf = ref false             (* -dinterf *)
let dump_prefer = ref false             (* -dprefer *)
let dump_regalloc = ref false           (* -dalloc *)
let dump_reload = ref false             (* -dreload *)
let dump_scheduling = ref false         (* -dscheduling *)
let dump_linear = ref false             (* -dlinear *)
let keep_startup_file = ref false       (* -dstartup *)
let dump_combine = ref false            (* -dcombine *)
let native_code = ref false             (* set to true under ocamlopt *)
let inline_threshold = ref 10
let force_slash = ref false             (* for ocamldep *)

let dont_write_files = ref false        (* set to true under ocamldoc *)

let std_include_flag prefix =
  if !no_std_include then ""
  else (prefix ^ (Filename.quote Config.standard_library))
;;

let std_include_dir () =
  if !no_std_include then [] else [Config.standard_library]
;;

let shared = ref false (* -shared *)
let dlcode = ref true (* not -nodynlink *)

let runtime_variant = ref "";;      (* -runtime-variant *)

let keep_docs = ref false              (* -keep-docs *)
let keep_locs = ref false              (* -keep-locs *)
let unsafe_string = ref true;;         (* -safe-string / -unsafe-string *)


#if undefined  BS_NO_COMPILER_PATCH then 
type mli_status = Mli_na | Mli_exists | Mli_non_exists
let no_implicit_current_dir = ref false
let assume_no_mli = ref Mli_na
let record_event_when_debug = ref true (* turned off in BuckleScript*)
let bs_vscode = 
    try ignore @@ Sys.getenv "BS_VSCODE" ; true with _ -> false
    (* We get it from environment variable mostly due to 
       we don't want to rebuild when flip on or off
    *)
let dont_record_crc_unit : string option ref = ref None
let bs_only = ref false
let no_assert_false = ref false
#end

type color_setting = Auto | Always | Never
let parse_color_setting = function
  | "auto" -> Some Auto
  | "always" -> Some Always
  | "never" -> Some Never
  | _ -> None
let color = ref Auto ;; (* -color *)

