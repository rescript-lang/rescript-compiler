(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Pierre Weis && Damien Doligez, INRIA Rocquencourt        *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* When you change this, you need to update the documentation:
   - man/ocamlc.m   in ocaml
   - man/ocamlopt.m in ocaml
   - manual/cmds/comp.etex   in the doc sources
   - manual/cmds/native.etex in the doc sources
*)

type t =
  | Comment_start                           (*  1 *)
  | Comment_not_end                         (*  2 *)
  | Deprecated of string                    (*  3 *)
  | Fragile_match of string                 (*  4 *)
  | Partial_application                     (*  5 *)
  | Labels_omitted                          (*  6 *)
  | Method_override of string list          (*  7 *)
  | Partial_match of string                 (*  8 *)
  | Non_closed_record_pattern of string     (*  9 *)
  | Statement_type                          (* 10 *)
  | Unused_match                            (* 11 *)
  | Unused_pat                              (* 12 *)
  | Instance_variable_override of string list (* 13 *)
  | Illegal_backslash                       (* 14 *)
  | Implicit_public_methods of string list  (* 15 *)
  | Unerasable_optional_argument            (* 16 *)
  | Undeclared_virtual_method of string     (* 17 *)
  | Not_principal of string                 (* 18 *)
  | Without_principality of string          (* 19 *)
  | Unused_argument                         (* 20 *)
  | Nonreturning_statement                  (* 21 *)
  | Preprocessor of string                  (* 22 *)
  | Useless_record_with                     (* 23 *)
  | Bad_module_name of string               (* 24 *)
  | All_clauses_guarded                     (* 25 *)
  | Unused_var of string                    (* 26 *)
  | Unused_var_strict of string             (* 27 *)
  | Wildcard_arg_to_constant_constr         (* 28 *)
  | Eol_in_string                           (* 29 *)
  | Duplicate_definitions of string * string * string * string (*30 *)
  | Multiple_definition of string * string * string (* 31 *)
  | Unused_value_declaration of string      (* 32 *)
  | Unused_open of string                   (* 33 *)
  | Unused_type_declaration of string       (* 34 *)
  | Unused_for_index of string              (* 35 *)
  | Unused_ancestor of string               (* 36 *)
  | Unused_constructor of string * bool * bool  (* 37 *)
  | Unused_extension of string * bool * bool    (* 38 *)
  | Unused_rec_flag                         (* 39 *)
  | Name_out_of_scope of string * string list * bool (* 40 *)
  | Ambiguous_name of string list * string list *  bool    (* 41 *)
  | Disambiguated_name of string            (* 42 *)
  | Nonoptional_label of string             (* 43 *)
  | Open_shadow_identifier of string * string (* 44 *)
  | Open_shadow_label_constructor of string * string (* 45 *)
  | Bad_env_variable of string * string     (* 46 *)
  | Attribute_payload of string * string    (* 47 *)
  | Eliminated_optional_arguments of string list (* 48 *)
  | No_cmi_file of string                   (* 49 *)
  | Bad_docstring of bool                   (* 50 *)

  | Bs_unused_attribute of string           (* 101 *)
  | Bs_polymorphic_comparison               (* 102 *)
  | Bs_ffi_warning of string                (* 103 *)
  | Bs_derive_warning of string             (* 104 *)
;;

(* If you remove a warning, leave a hole in the numbering.  NEVER change
   the numbers of existing warnings.
   If you add a new warning, add it at the end with a new number;
   do NOT reuse one of the holes.
*)

let number = function
  | Comment_start -> 1
  | Comment_not_end -> 2
  | Deprecated _ -> 3
  | Fragile_match _ -> 4
  | Partial_application -> 5
  | Labels_omitted -> 6
  | Method_override _ -> 7
  | Partial_match _ -> 8
  | Non_closed_record_pattern _ -> 9
  | Statement_type -> 10
  | Unused_match -> 11
  | Unused_pat -> 12
  | Instance_variable_override _ -> 13
  | Illegal_backslash -> 14
  | Implicit_public_methods _ -> 15
  | Unerasable_optional_argument -> 16
  | Undeclared_virtual_method _ -> 17
  | Not_principal _ -> 18
  | Without_principality _ -> 19
  | Unused_argument -> 20
  | Nonreturning_statement -> 21
  | Preprocessor _ -> 22
  | Useless_record_with -> 23
  | Bad_module_name _ -> 24
  | All_clauses_guarded -> 25
  | Unused_var _ -> 26
  | Unused_var_strict _ -> 27
  | Wildcard_arg_to_constant_constr -> 28
  | Eol_in_string -> 29
  | Duplicate_definitions _ -> 30
  | Multiple_definition _ -> 31
  | Unused_value_declaration _ -> 32
  | Unused_open _ -> 33
  | Unused_type_declaration _ -> 34
  | Unused_for_index _ -> 35
  | Unused_ancestor _ -> 36
  | Unused_constructor _ -> 37
  | Unused_extension _ -> 38
  | Unused_rec_flag -> 39
  | Name_out_of_scope _ -> 40
  | Ambiguous_name _ -> 41
  | Disambiguated_name _ -> 42
  | Nonoptional_label _ -> 43
  | Open_shadow_identifier _ -> 44
  | Open_shadow_label_constructor _ -> 45
  | Bad_env_variable _ -> 46
  | Attribute_payload _ -> 47
  | Eliminated_optional_arguments _ -> 48
  | No_cmi_file _ -> 49
  | Bad_docstring _ -> 50

  | Bs_unused_attribute _ -> 101
  | Bs_polymorphic_comparison -> 102
  | Bs_ffi_warning _ -> 103
  | Bs_derive_warning _ -> 104
;;

let last_warning_number = 104
(* Must be the max number returned by the [number] function. *)
let letter_all = 
  let rec loop i = if i = 0 then [] else i :: loop (i - 1) in
  loop last_warning_number

let letter = function
  | 'a' ->
    letter_all
  | 'b' -> []
  | 'c' -> [1; 2]
  | 'd' -> [3]
  | 'e' -> [4]
  | 'f' -> [5]
  | 'g' -> []
  | 'h' -> []
  | 'i' -> []
  | 'j' -> []
  | 'k' -> [32; 33; 34; 35; 36; 37; 38; 39]
  | 'l' -> [6]
  | 'm' -> [7]
  | 'n' -> []
  | 'o' -> []
  | 'p' -> [8]
  | 'q' -> []
  | 'r' -> [9]
  | 's' -> [10]
  | 't' -> []
  | 'u' -> [11; 12]
  | 'v' -> [13]
  | 'w' -> []
  | 'x' -> [14; 15; 16; 17; 18; 19; 20; 21; 22; 23; 24; 25; 30]
  | 'y' -> [26]
  | 'z' -> [27]
  | _ -> assert false
;;

type state =
  {
    active: bool array;
    error: bool array;
  }

let current =
  ref
    {
      active = Array.make (last_warning_number + 1) true;
      error = Array.make (last_warning_number + 1) false;
    }

let backup () = !current

let restore x = current := x

let is_active x = (!current).active.(number x);;
let is_error x = (!current).error.(number x);;

let parse_opt error active flags s =
  let set i = flags.(i) <- true in
  let clear i = flags.(i) <- false in
  let set_all i = active.(i) <- true; error.(i) <- true in
  let error () = raise (Arg.Bad "Ill-formed list of warnings") in
  let rec get_num n i =
    if i >= String.length s then i, n
    else match s.[i] with
    | '0'..'9' -> get_num (10 * n + Char.code s.[i] - Char.code '0') (i + 1)
    | _ -> i, n
  in
  let get_range i =
    let i, n1 = get_num 0 i in
    if i + 2 < String.length s && s.[i] = '.' && s.[i + 1] = '.' then
      let i, n2 = get_num 0 (i + 2) in
      if n2 < n1 then error ();
      i, n1, n2
    else
      i, n1, n1
  in
  let rec loop i =
    if i >= String.length s then () else
    match s.[i] with
    | 'A' .. 'Z' ->
       List.iter set (letter (Char.lowercase s.[i]));
       loop (i+1)
    | 'a' .. 'z' ->
       List.iter clear (letter s.[i]);
       loop (i+1)
    | '+' -> loop_letter_num set (i+1)
    | '-' -> loop_letter_num clear (i+1)
    | '@' -> loop_letter_num set_all (i+1)
    | c -> error ()
  and loop_letter_num myset i =
    if i >= String.length s then error () else
    match s.[i] with
    | '0' .. '9' ->
        let i, n1, n2 = get_range i in
        for n = n1 to min n2 last_warning_number do myset n done;
        loop i
    | 'A' .. 'Z' ->
       List.iter myset (letter (Char.lowercase s.[i]));
       loop (i+1)
    | 'a' .. 'z' ->
       List.iter myset (letter s.[i]);
       loop (i+1)
    | _ -> error ()
  in
  loop 0
;;

let parse_options errflag s =
  let error = Array.copy (!current).error in
  let active = Array.copy (!current).active in
  parse_opt error active (if errflag then error else active) s;
  current := {error; active}

(* If you change these, don't forget to change them in man/ocamlc.m *)
let defaults_w = "+a-4-6-7-9-27-29-32..39-41..42-44-45-48-50-102";;
let defaults_warn_error = "-a";;

let () = parse_options false defaults_w;;
let () = parse_options true defaults_warn_error;;

let message = function
  | Comment_start -> "this is the start of a comment."
  | Comment_not_end -> "this is not the end of a comment."
  | Deprecated s -> "deprecated: " ^ s
  | Fragile_match "" ->
      "this pattern-matching is fragile."
  | Fragile_match s ->
      "this pattern-matching is fragile.\n\
       It will remain exhaustive when constructors are added to type " ^ s ^ "."
  | Partial_application ->
      "this function application is partial,\n\
       maybe some arguments are missing."
  | Labels_omitted ->
      "labels were omitted in the application of this function."
  | Method_override [lab] ->
      "the method " ^ lab ^ " is overridden."
  | Method_override (cname :: slist) ->
      String.concat " "
        ("the following methods are overridden by the class"
         :: cname  :: ":\n " :: slist)
  | Method_override [] -> assert false
  | Partial_match "" -> "this pattern-matching is not exhaustive."
  | Partial_match s ->
      "this pattern-matching is not exhaustive.\n\
       Here is an example of a value that is not matched:\n" ^ s
  | Non_closed_record_pattern s ->
      "the following labels are not bound in this record pattern:\n" ^ s ^
      "\nEither bind these labels explicitly or add '; _' to the pattern."
  | Statement_type ->
      "this expression should have type unit."
  | Unused_match -> "this match case is unused."
  | Unused_pat   -> "this sub-pattern is unused."
  | Instance_variable_override [lab] ->
      "the instance variable " ^ lab ^ " is overridden.\n" ^
      "The behaviour changed in ocaml 3.10 (previous behaviour was hiding.)"
  | Instance_variable_override (cname :: slist) ->
      String.concat " "
        ("the following instance variables are overridden by the class"
         :: cname  :: ":\n " :: slist) ^
      "\nThe behaviour changed in ocaml 3.10 (previous behaviour was hiding.)"
  | Instance_variable_override [] -> assert false
  | Illegal_backslash -> "illegal backslash escape in string."
  | Implicit_public_methods l ->
      "the following private methods were made public implicitly:\n "
      ^ String.concat " " l ^ "."
  | Unerasable_optional_argument -> "this optional argument cannot be erased."
  | Undeclared_virtual_method m -> "the virtual method "^m^" is not declared."
  | Not_principal s -> s^" is not principal."
  | Without_principality s -> s^" without principality."
  | Unused_argument -> "this argument will not be used by the function."
  | Nonreturning_statement ->
      "this statement never returns (or has an unsound type.)"
  | Preprocessor s -> s
  | Useless_record_with ->
      "all the fields are explicitly listed in this record:\n\
       the 'with' clause is useless."
  | Bad_module_name (modname) ->
      "bad source file name: \"" ^ modname ^ "\" is not a valid module name."
  | All_clauses_guarded ->
      "bad style, all clauses in this pattern-matching are guarded."
  | Unused_var v | Unused_var_strict v -> "unused variable " ^ v ^ "."
  | Wildcard_arg_to_constant_constr ->
     "wildcard pattern given as argument to a constant constructor"
  | Eol_in_string ->
     "unescaped end-of-line in a string constant (non-portable code)"
  | Duplicate_definitions (kind, cname, tc1, tc2) ->
      Printf.sprintf "the %s %s is defined in both types %s and %s."
        kind cname tc1 tc2
  | Multiple_definition(modname, file1, file2) ->
      Printf.sprintf
        "files %s and %s both define a module named %s"
        file1 file2 modname
  | Unused_value_declaration v -> "unused value " ^ v ^ "."
  | Unused_open s -> "unused open " ^ s ^ "."
  | Unused_type_declaration s -> "unused type " ^ s ^ "."
  | Unused_for_index s -> "unused for-loop index " ^ s ^ "."
  | Unused_ancestor s -> "unused ancestor variable " ^ s ^ "."
  | Unused_constructor (s, false, false) -> "unused constructor " ^ s ^ "."
  | Unused_constructor (s, true, _) ->
      "constructor " ^ s ^
      " is never used to build values.\n\
        (However, this constructor appears in patterns.)"
  | Unused_constructor (s, false, true) ->
      "constructor " ^ s ^
      " is never used to build values.\n\
        Its type is exported as a private type."
  | Unused_extension (s, false, false) ->
      "unused extension constructor " ^ s ^ "."
  | Unused_extension (s, true, _) ->
      "extension constructor " ^ s ^
      " is never used to build values.\n\
        (However, this constructor appears in patterns.)"
  | Unused_extension (s, false, true) ->
      "extension constructor " ^ s ^
      " is never used to build values.\n\
        It is exported or rebound as a private extension."
  | Unused_rec_flag ->
      "unused rec flag."
  | Name_out_of_scope (ty, [nm], false) ->
      nm ^ " was selected from type " ^ ty ^
      ".\nIt is not visible in the current scope, and will not \n\
       be selected if the type becomes unknown."
  | Name_out_of_scope (_, _, false) -> assert false
  | Name_out_of_scope (ty, slist, true) ->
      "this record of type "^ ty ^" contains fields that are \n\
       not visible in the current scope: "
      ^ String.concat " " slist ^ ".\n\
       They will not be selected if the type becomes unknown."
  | Ambiguous_name ([s], tl, false) ->
      s ^ " belongs to several types: " ^ String.concat " " tl ^
      "\nThe first one was selected. Please disambiguate if this is wrong."
  | Ambiguous_name (_, _, false) -> assert false
  | Ambiguous_name (slist, tl, true) ->
      "these field labels belong to several types: " ^
      String.concat " " tl ^
      "\nThe first one was selected. Please disambiguate if this is wrong."
  | Disambiguated_name s ->
      "this use of " ^ s ^ " required disambiguation."
  | Nonoptional_label s ->
      "the label " ^ s ^ " is not optional."
  | Open_shadow_identifier (kind, s) ->
      Printf.sprintf
        "this open statement shadows the %s identifier %s (which is later used)"
        kind s
  | Open_shadow_label_constructor (kind, s) ->
      Printf.sprintf
        "this open statement shadows the %s %s (which is later used)"
        kind s
  | Bad_env_variable (var, s) ->
      Printf.sprintf "illegal environment variable %s : %s" var s
  | Attribute_payload (a, s) ->
      Printf.sprintf "illegal payload for attribute '%s'.\n%s" a s
  | Eliminated_optional_arguments sl ->
      Printf.sprintf "implicit elimination of optional argument%s %s"
        (if List.length sl = 1 then "" else "s")
        (String.concat ", " sl)
  | No_cmi_file s ->
      "no cmi file was found in path for module " ^ s
  | Bad_docstring unattached ->
      if unattached then "unattached documentation comment (ignored)"
      else "ambiguous documentation comment"
  | Bs_unused_attribute s ->
      "Unused BuckleScript attribute: " ^ s
  | Bs_polymorphic_comparison ->
      "polymorphic comparison introduced (maybe unsafe)"
  | Bs_ffi_warning s ->
      "BuckleScript FFI warning: " ^ s
  | Bs_derive_warning s ->
      "BuckleScript bs.deriving warning: " ^ s 
;;

let nerrors = ref 0;;

let print ppf w =
  let msg = message w in
  let num = number w in
  Format.fprintf ppf "%d: %s" num msg;
  Format.pp_print_flush ppf ();
  if (!current).error.(num) then incr nerrors
;;

#if undefined BS_NO_COMPILER_PATCH then
(* used by super-errors. Copied from the `print` above *)
let super_print message ppf w =
  let msg = message w in
  let num = number w in
  Format.fprintf ppf "%s" msg;
  Format.pp_print_flush ppf ();
  if (!current).error.(num) then incr nerrors
;;
#end

exception Errors of int;;

let check_fatal () =
  if !nerrors > 0 then begin
    let e = Errors !nerrors in
    nerrors := 0;
    raise e;
  end;
;;

let descriptions =
  [
    1, "Suspicious-looking start-of-comment mark.";
    2, "Suspicious-looking end-of-comment mark.";
    3, "Deprecated feature.";
    4, "Fragile pattern matching: matching that will remain complete even\n\
   \    if additional constructors are added to one of the variant types\n\
   \    matched.";
    5, "Partially applied function: expression whose result has function\n\
   \    type and is ignored.";
    6, "Label omitted in function application.";
    7, "Method overridden.";
    8, "Partial match: missing cases in pattern-matching.";
    9, "Missing fields in a record pattern.";
   10, "Expression on the left-hand side of a sequence that doesn't have type\n\
   \    \"unit\" (and that is not a function, see warning number 5).";
   11, "Redundant case in a pattern matching (unused match case).";
   12, "Redundant sub-pattern in a pattern-matching.";
   13, "Instance variable overridden.";
   14, "Illegal backslash escape in a string constant.";
   15, "Private method made public implicitly.";
   16, "Unerasable optional argument.";
   17, "Undeclared virtual method.";
   18, "Non-principal type.";
   19, "Type without principality.";
   20, "Unused function argument.";
   21, "Non-returning statement.";
   22, "Proprocessor warning.";
   23, "Useless record \"with\" clause.";
   24, "Bad module name: the source file name is not a valid OCaml module \
        name.";
   25, "Pattern-matching with all clauses guarded.  Exhaustiveness cannot be\n\
   \    checked.";
   26, "Suspicious unused variable: unused variable that is bound\n\
   \    with \"let\" or \"as\", and doesn't start with an underscore (\"_\")\n\
   \    character.";
   27, "Innocuous unused variable: unused variable that is not bound with\n\
   \    \"let\" nor \"as\", and doesn't start with an underscore (\"_\")\n\
   \    character.";
   28, "Wildcard pattern given as argument to a constant constructor.";
   29, "Unescaped end-of-line in a string constant (non-portable code).";
   30, "Two labels or constructors of the same name are defined in two\n\
   \    mutually recursive types.";
   31, "A module is linked twice in the same executable.";
   32, "Unused value declaration.";
   33, "Unused open statement.";
   34, "Unused type declaration.";
   35, "Unused for-loop index.";
   36, "Unused ancestor variable.";
   37, "Unused constructor.";
   38, "Unused extension constructor.";
   39, "Unused rec flag.";
   40, "Constructor or label name used out of scope.";
   41, "Ambiguous constructor or label name.";
   42, "Disambiguated constructor or label name.";
   43, "Nonoptional label applied as optional.";
   44, "Open statement shadows an already defined identifier.";
   45, "Open statement shadows an already defined label or constructor.";
   46, "Error in environment variable.";
   47, "Illegal attribute payload.";
   48, "Implicit elimination of optional arguments.";
   49, "Missing cmi file when looking up module alias.";
   50, "Unexpected documentation comment.";
   101,"Unused bs attributes";
  ]
;;

let help_warnings () =
  List.iter (fun (i, s) -> Printf.printf "%3i %s\n" i s) descriptions;
  print_endline "  A all warnings";
  for i = Char.code 'b' to Char.code 'z' do
    let c = Char.chr i in
    match letter c with
    | [] -> ()
    | [n] ->
        Printf.printf "  %c warning %i\n" (Char.uppercase c) n
    | l ->
        Printf.printf "  %c warnings %s.\n"
          (Char.uppercase c)
          (String.concat ", " (List.map string_of_int l))
  done;
  exit 0
;;
