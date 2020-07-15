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


let process_interface_file ppf name =
  Js_implementation.interface ppf name 
  ~parser:Pparse_driver.parse_interface
  (Compenv.output_prefix name)
let process_implementation_file ppf name =
  Js_implementation.implementation ppf name 
  ~parser:Pparse_driver.parse_implementation
  (Compenv.output_prefix name)


let setup_reason_error_printer () = 
  Lazy.force Super_main.setup;  
  Lazy.force Reason_outcome_printer_main.setup

let setup_napkin_error_printer () =  
  Js_config.napkin := true;
  Lazy.force Super_main.setup;  
  Lazy.force Napkin_outcome_printer.setup

let handle_reason (type a) (kind : a Ml_binary.kind) sourcefile ppf opref = 
  setup_reason_error_printer ();
  let tmpfile =  Ast_reason_pp.pp sourcefile in   
  (match kind with 
   | Ml_binary.Ml -> 
     Js_implementation.implementation
       ~parser:(fun file_in -> 
           let in_chan = open_in_bin file_in in 
           let ast = Ml_binary.read_ast Ml in_chan in 
           close_in in_chan; ast 
         )
       ppf  tmpfile opref    

   | Ml_binary.Mli ->
     Js_implementation.interface 
       ~parser:(fun file_in -> 
           let in_chan = open_in_bin file_in in 
           let ast = Ml_binary.read_ast Mli in_chan in 
           close_in in_chan; ast 
         )
       ppf  tmpfile opref ;    );
  Ast_reason_pp.clean tmpfile 

  
type valid_input = 
  | Ml 
  | Mli
  | Re
  | Rei
  | Res
  | Resi
  | Resast
  | Resiast
  | Mlast    
  | Mliast 
  | Reast
  | Reiast
  | Mlmap
  | Cmi
  | Unknown
let bad_arg s = raise_notrace (Arg.Bad(s))

(** This is per-file based, 
    when [ocamlc] [-c -o another_dir/xx.cmi] 
    it will return (another_dir/xx)
*)    

let classify_input ext = 

  match () with 
  | _ when ext = Literals.suffix_ml ->   
    Ml
  | _ when ext = Literals.suffix_re ->
    Re
  | _ when ext = !Config.interface_suffix ->
    Mli  
  | _ when ext = Literals.suffix_rei ->
    Rei
  | _ when ext =  Literals.suffix_mlast ->
    Mlast 
  | _ when ext = Literals.suffix_mliast ->
    Mliast
  | _ when ext = Literals.suffix_reast ->
    Reast 
  | _ when ext = Literals.suffix_reiast ->
    Reiast
  | _ when ext =  Literals.suffix_mlmap ->
    Mlmap 
  | _ when ext =  Literals.suffix_cmi ->
    Cmi
  | _ when ext = Literals.suffix_res -> 
    Res
  | _ when ext = Literals.suffix_resi -> 
    Resi    
  | _ when ext = Literals.suffix_resast -> Resast   
  | _ when ext = Literals.suffix_resiast -> Resiast
  | _ -> Unknown

let process_file ppf sourcefile = 
  (* This is a better default then "", it will be changed later 
     The {!Location.input_name} relies on that we write the binary ast 
     properly
  *)
  Location.set_input_name  sourcefile;  
  let ext = Ext_filename.get_extension_maybe sourcefile in 
  let input = classify_input ext in 
  let opref = Compenv.output_prefix sourcefile in 
  match input with 
  | Re -> handle_reason Ml sourcefile ppf opref     
  | Rei ->
    handle_reason Mli sourcefile ppf opref 
  | Reiast 
    -> 
    setup_reason_error_printer ();
    Js_implementation.interface_mliast ppf sourcefile opref   
  | Reast 
    -> 
    setup_reason_error_printer ();
    Js_implementation.implementation_mlast ppf sourcefile opref
  | Res -> 
    setup_napkin_error_printer ();
    Js_implementation.implementation 
      ~parser:Napkin_driver.parse_implementation
      ppf sourcefile opref 
  | Resi ->   
    setup_napkin_error_printer ();
    Js_implementation.interface 
      ~parser:Napkin_driver.parse_interface
      ppf sourcefile opref       
  | Ml ->
    Js_implementation.implementation 
    ~parser:Pparse_driver.parse_implementation
    ppf sourcefile opref 
  | Mli  ->   
    Js_implementation.interface 
    ~parser:Pparse_driver.parse_interface
    ppf sourcefile opref   
  | Resiast
    ->   
    setup_napkin_error_printer ();
    Js_implementation.interface_mliast ppf sourcefile opref 
  | Mliast 
    -> Js_implementation.interface_mliast ppf sourcefile opref 
  | Resast  
    ->
    setup_napkin_error_printer ();
    Js_implementation.implementation_mlast ppf sourcefile opref
  | Mlast 
    -> Js_implementation.implementation_mlast ppf sourcefile opref
  | Mlmap 
    -> Js_implementation.implementation_map ppf sourcefile opref
  | Cmi
    ->
    let cmi_sign = (Cmi_format.read_cmi sourcefile).cmi_sign in 
    Printtyp.signature Format.std_formatter cmi_sign ; 
    Format.pp_print_newline Format.std_formatter ()      
  | Unknown -> 
    bad_arg ("don't know what to do with " ^ sourcefile)
let usage = "Usage: bsc <options> <files>\nOptions are:"

let ppf = Format.err_formatter
let ppx_files = ref []
(* Error messages to standard error formatter *)

let anonymous filename =
  Compenv.readenv ppf 
    (Before_compile filename); 
  if !Js_config.as_ppx then ppx_files := filename :: !ppx_files  
  else process_file ppf filename

(** used by -impl -intf *)
let impl filename =
  Compenv.readenv ppf 
    (Before_compile filename)
  ; process_implementation_file ppf filename;;
let intf filename =
  Compenv.readenv ppf 
    (Before_compile filename)
  ; process_interface_file ppf filename;;



let eval (s : string) ~suffix =
  let tmpfile = Filename.temp_file "eval" suffix in 
  Ext_io.write_file tmpfile s;   
  anonymous  tmpfile;
  Ast_reason_pp.clean tmpfile
  

(* let (//) = Filename.concat *)




                       
let define_variable s =
  match Ext_string.split ~keep_empty:true s '=' with
  | [key; v] -> 
    if not (Lexer.define_key_value key v)  then 
       bad_arg ("illegal definition: " ^ s)
  | _ -> bad_arg ("illegal definition: " ^ s)

let print_standard_library () = 
  let (//) = Filename.concat in   
  let standard_library = 
    Filename.dirname Sys.executable_name
    // Filename.parent_dir_name // "lib"// "ocaml"  in 
  print_string standard_library; print_newline(); 
  exit 0  

let bs_version_string = 
  "BuckleScript " ^ Bs_version.version ^
  " ( Using OCaml:" ^ Config.version ^ " )" 

let print_version_string () = 
#if undefined BS_RELEASE_BUILD then 
    print_string "DEV VERSION: ";
#end  
    print_endline bs_version_string;
    exit 0 
  

let buckle_script_flags : (string * Arg.spec * string) list =
  ["-bs-super-errors",
     Unit 
      (* needs to be set here instead of, say, setting a
        Js_config.better_errors flag; otherwise, when `anonymous` runs, we
        don't have time to set the custom printer before it starts outputting
        warnings *)
      (fun _ -> Lazy.force Super_main.setup)
     ,
   " Better error message combined with other tools "
  ;
  "-unboxed-types",
     Set Clflags.unboxed_types,
    " unannotated unboxable types will be unboxed"
  ;
  "-bs-re-out",
     Unit (fun _ -> Lazy.force Reason_outcome_printer_main.setup),
   " Print compiler output in Reason syntax"
  ;
  "-bs-jsx",
     String (fun i -> 
      (if i <> "3" then bad_arg (" Not supported jsx version : " ^  i));
      Js_config.jsx_version := 3),
    " Set jsx version"
  ;
  "-bs-refmt",
     String (fun s -> Js_config.refmt := Some s),
    " Set customized refmt path"
  ;
    "-bs-gentype",
     String (fun s -> Clflags.bs_gentype := Some s),
    " Pass gentype command"
  ;
  "-bs-suffix",
     Set Js_config.bs_suffix,
    " Set suffix to .bs.js"
  ;
  "-bs-no-implicit-include",  Set Clflags.no_implicit_current_dir
  , " Don't include current dir implicitly"
  ;
  "-bs-read-cmi",  Unit (fun _ -> Clflags.assume_no_mli := Mli_exists), 
    " (internal) Assume mli always exist "
  ;
  "-bs-D",  String define_variable,
     " Define conditional variable e.g, -D DEBUG=true"
  ;
  
  "-bs-unsafe-empty-array",  Clear Js_config.mono_empty_array,
    " Allow [||] to be polymorphic"
  ;
  "-nostdlib",  Set Js_config.no_stdlib,
    " Don't use stdlib"
  ;
  "-bs-internal-check",  Unit (Bs_cmi_load.check ),
    " Built in check corrupted data"
  ;  
  "-bs-list-conditionals",
    Unit (fun () -> Lexer.list_variables Format.err_formatter),
   " List existing conditional variables"
  ;
  
    "-bs-binary-ast",  Set Js_config.binary_ast,
    " Generate binary .mli_ast and ml_ast"
  ;
  
    "-bs-simple-binary-ast",  Set Js_config.simple_binary_ast,
    " Generate binary .mliast_simple and mlast_simple"
  ;
  "-bs-syntax-only", 
    Set Js_config.syntax_only,
   " only check syntax"  
  ;
  "-bs-eval", 
    String (fun  s -> eval s ~suffix:Literals.suffix_ml), 
   " (experimental) Set the string to be evaluated in OCaml syntax"
  ;
  
  "-e", String (fun  s -> eval s ~suffix:Literals.suffix_re), 
    " (experimental) Set the string to be evaluated in ReasonML syntax";
  
  "-bs-cmi-only", Set Js_config.cmi_only, " Stop after generating cmi file"
  ;
  
  "-bs-cmi", Set Js_config.force_cmi, " Not using cached cmi, always generate cmi"
  ;
  "-bs-cmj", Set Js_config.force_cmj, " Not using cached cmj, always generate cmj"
  ;    
  "-as-ppx", Set Js_config.as_ppx, " As ppx for editor integration"
  ;
  "-bs-g",
     Unit 
    (fun _ -> Js_config.debug := true;
      Lexer.replace_directive_bool "DEBUG" true
    ),
    " debug mode"
  ;
  
  
    "-bs-sort-imports",
     Set Js_config.sort_imports,
    " Sort the imports by lexical order so the output will be more stable (default false)"
  ;  
   "-bs-no-sort-imports", 
     Clear Js_config.sort_imports,
    " No sort (see -bs-sort-imports)"
  ;
  "-bs-package-name", 
    String Js_packages_state.set_package_name, 
   " set package name, useful when you want to produce npm packages"
  ;
  "-bs-ns", 
    String Js_packages_state.set_package_map, 
   " set package map, not only set package name but also use it as a namespace"    
  ;
  "-bs-no-version-header", 
    Set Js_config.no_version_header,
   " Don't print version header"
  ;
  "-bs-package-output", 
    String 
    Js_packages_state.update_npm_package_path, 
   " set npm-output-path: [opt_module]:path, for example: 'lib/cjs', 'amdjs:lib/amdjs', 'es6:lib/es6' "
  ;
  "-bs-no-builtin-ppx", 
    Set Js_config.no_builtin_ppx,
   "disable built-in ppx (internal use)"
  ;  
  "-bs-cross-module-opt", 
    Set Js_config.cross_module_inline, 
   "enable cross module inlining(experimental), default(false)"
  ;
   "-bs-no-cross-module-opt", 
     Clear Js_config.cross_module_inline, 
    "disable cross module inlining(experimental)"
  ;
  "-bs-diagnose",
    Set Js_config.diagnose, 
   " More verbose output"
  ;
  "-bs-no-check-div-by-zero",
    Clear Js_config.check_div_by_zero, 
   " unsafe mode, don't check div by zero and mod by zero"
  ;
  "-bs-noassertfalse",
     Set Clflags.no_assert_false,
    " no code for assert false"
  ;
  "-noassert", Set Clflags.noassert, 
  " Do not compile assertion checks"
  ;
  "-bs-loc",
     Set Clflags.dump_location, 
  " dont display location with -dtypedtree, -dparsetree"
  ;

  "-impl",  String
     (fun file  ->  Js_config.js_stdout := false;  impl file ),
   "<file>  Compile <file> as a .ml file"
  ;
  "-intf",  String 
     (fun file -> Js_config.js_stdout := false ; intf file),
   "<file>  Compile <file> as a .mli file"
  ;
  "-dtypedtree", Set Clflags.dump_typedtree, 
    " debug typedtree"
  ;
  "-dparsetree", Set Clflags.dump_parsetree,
    " debug parsetree"
  ;
  "-drawlambda", Set Clflags.dump_rawlambda,
    " debug raw lambda"
  ;
  "-dsource", Set Clflags.dump_source, " print source";
  "-fmt", String (fun input -> 
    let ext = classify_input (Ext_filename.get_extension_maybe input) in 
    let syntax = 
      match ext with 
      | Ml | Mli -> `ml
      | Res | Resi -> `res 
      | Re | Rei -> `refmt (Filename.concat (Filename.dirname Sys.executable_name) "refmt.exe") 
      | _ -> bad_arg ("don't know what to do with " ^ input) in   
    output_string stdout (Napkin_multi_printer.print syntax ~input)
    ),
    " (internal) format as Res syntax"
  ;
  "-where", Unit print_standard_library, 
  " Print location of standard library and exit"
  ;
   "-ppx", String (fun s -> Compenv.first_ppx := s ::!Compenv.first_ppx),
  "<command>  Pipe abstract syntax trees through preprocessor <command>"
  ;
  "-open", String (fun s -> Clflags.open_modules := s :: !Clflags.open_modules ),
     "<module>  Opens the module <module> before typing"
  ;
  "-verbose", Set Clflags.verbose, " Print calls to external commands"
  ;
  "-keep-locs", Set Clflags.keep_locs, " Keep locations in .cmi files"
  ;
  "-no-keep-locs", Clear Clflags.keep_locs, " Do not keep locations in .cmi files";
  "-nopervasives", Set Clflags.nopervasives, " (undocumented)";
  "-v", Unit print_version_string,
   " Print compiler version and location of standard library and exit";  
  "-version", Unit print_version_string, " Print version and exit";
  
  "-I", String (fun s -> Clflags.include_dirs := s :: !Clflags.include_dirs),
   "<dir>  Add <dir> to the list of include directories" ;
  "-pp", String (fun s -> Clflags.preprocessor := Some s),
   "<command>  Pipe sources through preprocessor <command>";
  "-absname", Set Location.absname, " Show absolute filenames in error messages";  
  "-bs-no-bin-annot",  Clear Clflags.binary_annotations, 
    " disable binary annotations (by default on)";
  "-i", Set Clflags.print_types, " Print inferred interface";  
  "-nolabels", Set Clflags.classic, " Ignore non-optional labels in types";  
  "-no-alias-deps", Set Clflags.transparent_modules, " Do not record dependencies for module aliases";
  "-o", String (fun s -> Clflags.output_name := Some s), "<file>  Set output file name to <file>";
  "-principal", Set Clflags.principal, " Check principality of type inference";  
  "-short-paths", Clear Clflags.real_paths, " Shorten paths in types";
  "-unsafe", Set Clflags.fast, " Do not compile bounds checking on array and string access";
  "-w", String (Warnings.parse_options false),
  "<list>  Enable or disable warnings according to <list>:\n\
  \        +<spec>   enable warnings in <spec>\n\
  \        -<spec>   disable warnings in <spec>\n\
  \        @<spec>   enable warnings in <spec> and treat them as errors\n\
  \     <spec> can be:\n\
  \        <num>             a single warning number\n\
  \        <num1>..<num2>    a range of consecutive warning numbers\n\
  \        <letter>          a predefined set\n\
  \     default setting is " ^ Bsc_warnings.defaults_w;  
  "-warn-error", String (Warnings.parse_options true),
  "<list>  Enable or disable error status for warnings according\n\
  \     to <list>.  See option -w for the syntax of <list>.\n\
  \     Default setting is " ^ Bsc_warnings.defaults_warn_error;
    "-warn-help", Unit (Warnings.help_warnings), " Show description of warning numbers";
    "-color", String (fun option -> match Clflags.parse_color_setting option with
| None -> ()
| Some setting -> Clflags.color := Some setting),

"  Enable or disable colors in compiler messages\n\
\    The following settings are supported:\n\
\      auto    use heuristics to enable colors only if supported\n\
\      always  enable colors\n\
\      never   disable colors\n\
\    The default setting is 'auto', and the current heuristic\n\
\    checks that the TERM environment variable exists and is\n\
\    not empty or \"dumb\", and that isatty(stderr) holds."
  ]


  
(** parse flags in bs.config *)
let file_level_flags_handler (e : Parsetree.expression option) = 
  match e with 
  | None -> ()
  | Some {pexp_desc = Pexp_array args ; pexp_loc} -> 
    let args = Array.of_list 
        (Sys.executable_name :: Ext_list.map  args (fun e -> 
             match e.pexp_desc with 
             | Pexp_constant (Pconst_string(name,_)) -> name 
             | _ -> Location.raise_errorf ~loc:e.pexp_loc "string literal expected" )) in               
    (try Arg.parse_argv ~current:(ref 0)
      args buckle_script_flags ignore usage
    with _ -> Location.prerr_warning pexp_loc (Preprocessor "invalid flags for bsc"))  
  | Some e -> 
    Location.raise_errorf ~loc:e.pexp_loc "string array expected"

let _ : unit =   
  Bs_conditional_initial.setup_env ();
  let flags = "flags" in 
  Ast_config.add_structure 
    flags file_level_flags_handler;    
  Ast_config.add_signature 
    flags file_level_flags_handler;    
  try
    Compenv.readenv ppf Before_args;
    Arg.parse buckle_script_flags anonymous usage;
    if !Js_config.as_ppx then 
      begin match !ppx_files with 
      | [output; input] ->
          Ppx_apply.apply_lazy
            ~source:input
            ~target:output
            Ppx_entry.rewrite_implementation
            Ppx_entry.rewrite_signature
      | _ -> bad_arg "Wrong format when use -as-ppx"
      end  
  with x -> 
    begin
#if undefined BS_RELEASE_BUILD then      
      Ext_obj.bt ();
#end
      Location.report_exception ppf x;
      exit 2
    end
