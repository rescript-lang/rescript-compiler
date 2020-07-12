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

(** Need sync up with {!Main_args} and {!Optmain} *)












let mk_intf_suffix f =
  "-intf-suffix", Arg.String f,
  "<string>  Suffix for interface files (default: .mli)"
;;



let mk_labels f =
  "-labels", Arg.Unit f, " Use commuting label mode"
;;

let mk_no_alias_deps f =
  "-no-alias-deps", Arg.Unit f,
  " Do not record dependencies for module aliases"
;;

let mk_no_app_funct f =
  "-no-app-funct", Arg.Unit f, " Deactivate applicative functors"
;;





let mk_nolabels f =
  "-nolabels", Arg.Unit f, " Ignore non-optional labels in types"
;;


let mk_o f =
  "-o", Arg.String f, "<file>  Set output file name to <file>"
;;




let mk_principal f =
  "-principal", Arg.Unit f, " Check principality of type inference"
;;

let mk_rectypes f =
  "-rectypes", Arg.Unit f, " Allow arbitrary recursive types"
;;



let mk_short_paths f =
  "-short-paths", Arg.Unit f, " Shorten paths in types"
;;




let mk_unsafe f =
  "-unsafe", Arg.Unit f,
  " Do not compile bounds checking on array and string access"
;;


let mk_w f =
  "-w", Arg.String f,
  Printf.sprintf
  "<list>  Enable or disable warnings according to <list>:\n\
  \        +<spec>   enable warnings in <spec>\n\
  \        -<spec>   disable warnings in <spec>\n\
  \        @<spec>   enable warnings in <spec> and treat them as errors\n\
  \     <spec> can be:\n\
  \        <num>             a single warning number\n\
  \        <num1>..<num2>    a range of consecutive warning numbers\n\
  \        <letter>          a predefined set\n\
  \     default setting is %S" Bsc_warnings.defaults_w
;;

let mk_warn_error f =
  "-warn-error", Arg.String f,
  Printf.sprintf
  "<list>  Enable or disable error status for warnings according\n\
  \     to <list>.  See option -w for the syntax of <list>.\n\
  \     Default setting is %S" Bsc_warnings.defaults_warn_error
;;

let mk_warn_help f =
  "-warn-help", Arg.Unit f, " Show description of warning numbers"
;;


let mk_color f =
  "-color", Arg.Symbol (["auto"; "always"; "never"], f),
  Printf.sprintf
  "  Enable or disable colors in compiler messages\n\
  \    The following settings are supported:\n\
  \      auto    use heuristics to enable colors only if supported\n\
  \      always  enable colors\n\
  \      never   disable colors\n\
  \    The default setting is 'auto', and the current heuristic\n\
  \    checks that the TERM environment variable exists and is\n\
  \    not empty or \"dumb\", and that isatty(stderr) holds."
;;












let ocaml_options = 
  let set r () = r := true in 
  let unset r () = r := false in 

  let _color option = 
    match Clflags.parse_color_setting option with
    | None -> ()
    | Some setting -> Clflags.color := Some setting in 

  let _intf_suffix s = Config.interface_suffix := s in 



  let _labels = unset Clflags.classic in 
  let _no_alias_deps = set Clflags.transparent_modules in 
  let _no_app_funct = unset Clflags.applicative_functors in 

  let _nolabels = set Clflags.classic in 
  let _o s = Clflags.output_name := Some s in 

  
  let _principal = set Clflags.principal in 
  let _rectypes = set Clflags.recursive_types in 
  let _short_paths = unset Clflags.real_paths in 
  let _unsafe = set Clflags.fast in 
  let _w = (Warnings.parse_options false) in
  let _warn_error = (Warnings.parse_options true) in
  let _warn_help = Warnings.help_warnings in
  



  [ 
    


    
    mk_color _color;
    mk_intf_suffix _intf_suffix;



    mk_labels _labels;
    mk_no_alias_deps _no_alias_deps;
    mk_no_app_funct _no_app_funct;

    mk_nolabels _nolabels;
    mk_o _o;

    

    mk_principal _principal;
    mk_rectypes _rectypes;
    mk_short_paths _short_paths;
    mk_unsafe _unsafe;
    


    mk_w _w;
    mk_warn_error _warn_error;
    mk_warn_help _warn_help;
  
    mk_color _color;
    
    
     ]

