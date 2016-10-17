module Config = Config_bsdep
module Terminfo : sig 
#1 "terminfo.mli"
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

(* Basic interface to the terminfo database *)

type status =
  | Uninitialised
  | Bad_term
  | Good_term of int  (* number of lines of the terminal *)
;;
external setup : out_channel -> status = "caml_terminfo_setup";;
external backup : int -> unit = "caml_terminfo_backup";;
external standout : bool -> unit = "caml_terminfo_standout";;
external resume : int -> unit = "caml_terminfo_resume";;

end = struct
#1 "terminfo.ml"
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

(* Basic interface to the terminfo database *)

type status =
  | Uninitialised
  | Bad_term
  | Good_term of int
;;
external setup : out_channel -> status = "caml_terminfo_setup";;
external backup : int -> unit = "caml_terminfo_backup";;
external standout : bool -> unit = "caml_terminfo_standout";;
external resume : int -> unit = "caml_terminfo_resume";;

end
module Warnings : sig 
#1 "warnings.mli"
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

open Format

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
  | Duplicate_definitions of string * string * string * string (* 30 *)
  | Multiple_definition of string * string * string (* 31 *)
  | Unused_value_declaration of string      (* 32 *)
  | Unused_open of string                   (* 33 *)
  | Unused_type_declaration of string       (* 34 *)
  | Unused_for_index of string              (* 35 *)
  | Unused_ancestor of string               (* 36 *)
  | Unused_constructor of string * bool * bool (* 37 *)
  | Unused_extension of string * bool * bool   (* 38 *)
  | Unused_rec_flag                         (* 39 *)
  | Name_out_of_scope of string * string list * bool   (* 40 *)
  | Ambiguous_name of string list * string list * bool (* 41 *)
  | Disambiguated_name of string            (* 42 *)
  | Nonoptional_label of string             (* 43 *)
  | Open_shadow_identifier of string * string (* 44 *)
  | Open_shadow_label_constructor of string * string (* 45 *)
  | Bad_env_variable of string * string     (* 46 *)
  | Attribute_payload of string * string    (* 47 *)
  | Eliminated_optional_arguments of string list (* 48 *)
  | No_cmi_file of string                   (* 49 *)
  | Bad_docstring of bool                   (* 50 *)
;;

val parse_options : bool -> string -> unit;;

val is_active : t -> bool;;
val is_error : t -> bool;;

val defaults_w : string;;
val defaults_warn_error : string;;

val print : formatter -> t -> unit;;

exception Errors of int;;

val check_fatal : unit -> unit;;

val help_warnings: unit -> unit

type state
val backup: unit -> state
val restore: state -> unit

end = struct
#1 "warnings.ml"
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
;;

let last_warning_number = 50
(* Must be the max number returned by the [number] function. *)

let letter = function
  | 'a' ->
     let rec loop i = if i = 0 then [] else i :: loop (i - 1) in
     loop last_warning_number
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
let defaults_w = "+a-4-6-7-9-27-29-32..39-41..42-44-45-48-50";;
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
;;

let nerrors = ref 0;;

let print ppf w =
  let msg = message w in
  let num = number w in
  Format.fprintf ppf "%d: %s" num msg;
  Format.pp_print_flush ppf ();
  if (!current).error.(num) then incr nerrors
;;

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

end
module Location : sig 
#1 "location.mli"
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

(* Source code locations (ranges of positions), used in parsetree. *)

open Format

type t = {
  loc_start: Lexing.position;
  loc_end: Lexing.position;
  loc_ghost: bool;
}

(* Note on the use of Lexing.position in this module.
   If [pos_fname = ""], then use [!input_name] instead.
   If [pos_lnum = -1], then [pos_bol = 0]. Use [pos_cnum] and
     re-parse the file to get the line and character numbers.
   Else all fields are correct.
*)

val none : t
(** An arbitrary value of type [t]; describes an empty ghost range. *)

val in_file : string -> t
(** Return an empty ghost range located in a given file. *)

val init : Lexing.lexbuf -> string -> unit
(** Set the file name and line number of the [lexbuf] to be the start
    of the named file. *)

val curr : Lexing.lexbuf -> t
(** Get the location of the current token from the [lexbuf]. *)

val symbol_rloc: unit -> t
val symbol_gloc: unit -> t

(** [rhs_loc n] returns the location of the symbol at position [n], starting
  at 1, in the current parser rule. *)
val rhs_loc: int -> t

val input_name: string ref
val input_lexbuf: Lexing.lexbuf option ref

val get_pos_info: Lexing.position -> string * int * int (* file, line, char *)
val print_loc: formatter -> t -> unit
val print_error: formatter -> t -> unit
val print_error_cur_file: formatter -> unit
val print_warning: t -> formatter -> Warnings.t -> unit
val formatter_for_warnings : formatter ref
val prerr_warning: t -> Warnings.t -> unit
val echo_eof: unit -> unit
val reset: unit -> unit

val warning_printer : (t -> formatter -> Warnings.t -> unit) ref
(** Hook for intercepting warnings. *)

val default_warning_printer : t -> formatter -> Warnings.t -> unit
(** Original warning printer for use in hooks. *)

val highlight_locations: formatter -> t list -> bool

type 'a loc = {
  txt : 'a;
  loc : t;
}

val mknoloc : 'a -> 'a loc
val mkloc : 'a -> t -> 'a loc

val print: formatter -> t -> unit
val print_filename: formatter -> string -> unit

val absolute_path: string -> string

val show_filename: string -> string
    (** In -absname mode, return the absolute path for this filename.
        Otherwise, returns the filename unchanged. *)


val absname: bool ref


(* Support for located errors *)

type error =
  {
    loc: t;
    msg: string;
    sub: error list;
    if_highlight: string; (* alternative message if locations are highlighted *)
  }

exception Error of error

val error: ?loc:t -> ?sub:error list -> ?if_highlight:string -> string -> error

val errorf: ?loc:t -> ?sub:error list -> ?if_highlight:string
            -> ('a, unit, string, error) format4 -> 'a

val raise_errorf: ?loc:t -> ?sub:error list -> ?if_highlight:string
            -> ('a, unit, string, 'b) format4 -> 'a

val error_of_printer: t -> (formatter -> 'a -> unit) -> 'a -> error

val error_of_printer_file: (formatter -> 'a -> unit) -> 'a -> error

val error_of_exn: exn -> error option

val register_error_of_exn: (exn -> error option) -> unit
  (* Each compiler module which defines a custom type of exception
     which can surface as a user-visible error should register
     a "printer" for this exception using [register_error_of_exn].
     The result of the printer is an [error] value containing
     a location, a message, and optionally sub-messages (each of them
     being located as well). *)

val report_error: formatter -> error -> unit

val error_reporter : (formatter -> error -> unit) ref
(** Hook for intercepting error reports. *)

val default_error_reporter : formatter -> error -> unit
(** Original error reporter for use in hooks. *)

val report_exception: formatter -> exn -> unit
  (* Reraise the exception if it is unknown. *)

end = struct
#1 "location.ml"
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

open Lexing

let absname = ref false
    (* This reference should be in Clflags, but it would create an additional
       dependency and make bootstrapping Camlp4 more difficult. *)

type t = { loc_start: position; loc_end: position; loc_ghost: bool };;

let in_file name =
  let loc = {
    pos_fname = name;
    pos_lnum = 1;
    pos_bol = 0;
    pos_cnum = -1;
  } in
  { loc_start = loc; loc_end = loc; loc_ghost = true }
;;

let none = in_file "_none_";;

let curr lexbuf = {
  loc_start = lexbuf.lex_start_p;
  loc_end = lexbuf.lex_curr_p;
  loc_ghost = false
};;

let init lexbuf fname =
  lexbuf.lex_curr_p <- {
    pos_fname = fname;
    pos_lnum = 1;
    pos_bol = 0;
    pos_cnum = 0;
  }
;;

let symbol_rloc () = {
  loc_start = Parsing.symbol_start_pos ();
  loc_end = Parsing.symbol_end_pos ();
  loc_ghost = false;
};;

let symbol_gloc () = {
  loc_start = Parsing.symbol_start_pos ();
  loc_end = Parsing.symbol_end_pos ();
  loc_ghost = true;
};;

let rhs_loc n = {
  loc_start = Parsing.rhs_start_pos n;
  loc_end = Parsing.rhs_end_pos n;
  loc_ghost = false;
};;

let input_name = ref "_none_"
let input_lexbuf = ref (None : lexbuf option)

(* Terminal info *)

let status = ref Terminfo.Uninitialised

let num_loc_lines = ref 0 (* number of lines already printed after input *)

let print_updating_num_loc_lines ppf f arg =
  let open Format in
  let out_functions = pp_get_formatter_out_functions ppf () in
  let out_string str start len =
    let rec count i c =
      if i = start + len then c
      else if String.get str i = '\n' then count (succ i) (succ c)
      else count (succ i) c in
    num_loc_lines := !num_loc_lines + count start 0 ;
    out_functions.out_string str start len in
  pp_set_formatter_out_functions ppf
    { out_functions with out_string } ;
  f ppf arg ;
  pp_print_flush ppf ();
  pp_set_formatter_out_functions ppf out_functions

(* Highlight the locations using standout mode. *)

let highlight_terminfo ppf num_lines lb locs =
  Format.pp_print_flush ppf ();  (* avoid mixing Format and normal output *)
  (* Char 0 is at offset -lb.lex_abs_pos in lb.lex_buffer. *)
  let pos0 = -lb.lex_abs_pos in
  (* Do nothing if the buffer does not contain the whole phrase. *)
  if pos0 < 0 then raise Exit;
  (* Count number of lines in phrase *)
  let lines = ref !num_loc_lines in
  for i = pos0 to lb.lex_buffer_len - 1 do
    if Bytes.get lb.lex_buffer i = '\n' then incr lines
  done;
  (* If too many lines, give up *)
  if !lines >= num_lines - 2 then raise Exit;
  (* Move cursor up that number of lines *)
  flush stdout; Terminfo.backup !lines;
  (* Print the input, switching to standout for the location *)
  let bol = ref false in
  print_string "# ";
  for pos = 0 to lb.lex_buffer_len - pos0 - 1 do
    if !bol then (print_string "  "; bol := false);
    if List.exists (fun loc -> pos = loc.loc_start.pos_cnum) locs then
      Terminfo.standout true;
    if List.exists (fun loc -> pos = loc.loc_end.pos_cnum) locs then
      Terminfo.standout false;
    let c = Bytes.get lb.lex_buffer (pos + pos0) in
    print_char c;
    bol := (c = '\n')
  done;
  (* Make sure standout mode is over *)
  Terminfo.standout false;
  (* Position cursor back to original location *)
  Terminfo.resume !num_loc_lines;
  flush stdout

(* Highlight the location by printing it again. *)

let highlight_dumb ppf lb loc =
  (* Char 0 is at offset -lb.lex_abs_pos in lb.lex_buffer. *)
  let pos0 = -lb.lex_abs_pos in
  (* Do nothing if the buffer does not contain the whole phrase. *)
  if pos0 < 0 then raise Exit;
  let end_pos = lb.lex_buffer_len - pos0 - 1 in
  (* Determine line numbers for the start and end points *)
  let line_start = ref 0 and line_end = ref 0 in
  for pos = 0 to end_pos do
    if Bytes.get lb.lex_buffer (pos + pos0) = '\n' then begin
      if loc.loc_start.pos_cnum > pos then incr line_start;
      if loc.loc_end.pos_cnum   > pos then incr line_end;
    end
  done;
  (* Print character location (useful for Emacs) *)
  Format.fprintf ppf "Characters %i-%i:@."
                 loc.loc_start.pos_cnum loc.loc_end.pos_cnum;
  (* Print the input, underlining the location *)
  Format.pp_print_string ppf "  ";
  let line = ref 0 in
  let pos_at_bol = ref 0 in
  for pos = 0 to end_pos do
    match Bytes.get lb.lex_buffer (pos + pos0) with
    | '\n' ->
      if !line = !line_start && !line = !line_end then begin
        (* loc is on one line: underline location *)
        Format.fprintf ppf "@.  ";
        for _i = !pos_at_bol to loc.loc_start.pos_cnum - 1 do
          Format.pp_print_char ppf ' '
        done;
        for _i = loc.loc_start.pos_cnum to loc.loc_end.pos_cnum - 1 do
          Format.pp_print_char ppf '^'
        done
      end;
      if !line >= !line_start && !line <= !line_end then begin
        Format.fprintf ppf "@.";
        if pos < loc.loc_end.pos_cnum then Format.pp_print_string ppf "  "
      end;
      incr line;
      pos_at_bol := pos + 1
    | '\r' -> () (* discard *)
    | c ->
      if !line = !line_start && !line = !line_end then
        (* loc is on one line: print whole line *)
        Format.pp_print_char ppf c
      else if !line = !line_start then
        (* first line of multiline loc:
           print a dot for each char before loc_start *)
        if pos < loc.loc_start.pos_cnum then
          Format.pp_print_char ppf '.'
        else
          Format.pp_print_char ppf c
      else if !line = !line_end then
        (* last line of multiline loc: print a dot for each char
           after loc_end, even whitespaces *)
        if pos < loc.loc_end.pos_cnum then
          Format.pp_print_char ppf c
        else
          Format.pp_print_char ppf '.'
      else if !line > !line_start && !line < !line_end then
        (* intermediate line of multiline loc: print whole line *)
        Format.pp_print_char ppf c
  done

(* Highlight the location using one of the supported modes. *)

let rec highlight_locations ppf locs =
  match !status with
    Terminfo.Uninitialised ->
      status := Terminfo.setup stdout; highlight_locations ppf locs
  | Terminfo.Bad_term ->
      begin match !input_lexbuf with
        None -> false
      | Some lb ->
          let norepeat =
            try Sys.getenv "TERM" = "norepeat" with Not_found -> false in
          if norepeat then false else
            let loc1 = List.hd locs in
            try highlight_dumb ppf lb loc1; true
            with Exit -> false
      end
  | Terminfo.Good_term num_lines ->
      begin match !input_lexbuf with
        None -> false
      | Some lb ->
          try highlight_terminfo ppf num_lines lb locs; true
          with Exit -> false
      end

(* Print the location in some way or another *)

open Format

let absolute_path s = (* This function could go into Filename *)
  let open Filename in
  let s = if is_relative s then concat (Sys.getcwd ()) s else s in
  (* Now simplify . and .. components *)
  let rec aux s =
    let base = basename s in
    let dir = dirname s in
    if dir = s then dir
    else if base = current_dir_name then aux dir
    else if base = parent_dir_name then dirname (aux dir)
    else concat (aux dir) base
  in
  aux s

let show_filename file =
  if !absname then absolute_path file else file

let print_filename ppf file =
  Format.fprintf ppf "%s" (show_filename file)

let reset () =
  num_loc_lines := 0

let (msg_file, msg_line, msg_chars, msg_to, msg_colon) =
  ("File \"", "\", line ", ", characters ", "-", ":")

(* return file, line, char from the given position *)
let get_pos_info pos =
  (pos.pos_fname, pos.pos_lnum, pos.pos_cnum - pos.pos_bol)
;;

let print_loc ppf loc =
  let (file, line, startchar) = get_pos_info loc.loc_start in
  let endchar = loc.loc_end.pos_cnum - loc.loc_start.pos_cnum + startchar in
  if file = "//toplevel//" then begin
    if highlight_locations ppf [loc] then () else
      fprintf ppf "Characters %i-%i"
              loc.loc_start.pos_cnum loc.loc_end.pos_cnum
  end else begin
    fprintf ppf "%s%a%s%i" msg_file print_filename file msg_line line;
    if startchar >= 0 then
      fprintf ppf "%s%i%s%i" msg_chars startchar msg_to endchar
  end
;;

let print ppf loc =
  if loc.loc_start.pos_fname = "//toplevel//"
  && highlight_locations ppf [loc] then ()
  else fprintf ppf "%a%s@." print_loc loc msg_colon
;;

let print_error ppf loc =
  print ppf loc;
  fprintf ppf "Error: ";
;;

let print_error_cur_file ppf = print_error ppf (in_file !input_name);;

let default_warning_printer loc ppf w =
  if Warnings.is_active w then begin
    print ppf loc;
    fprintf ppf "Warning %a@." Warnings.print w
  end
;;

let warning_printer = ref default_warning_printer ;;

let print_warning loc ppf w =
  print_updating_num_loc_lines ppf (!warning_printer loc) w
;;

let formatter_for_warnings = ref err_formatter;;
let prerr_warning loc w = print_warning loc !formatter_for_warnings w;;

let echo_eof () =
  print_newline ();
  incr num_loc_lines

type 'a loc = {
  txt : 'a;
  loc : t;
}

let mkloc txt loc = { txt ; loc }
let mknoloc txt = mkloc txt none


type error =
  {
    loc: t;
    msg: string;
    sub: error list;
    if_highlight: string; (* alternative message if locations are highlighted *)
  }

let errorf ?(loc = none) ?(sub = []) ?(if_highlight = "") =
  Printf.ksprintf (fun msg -> {loc; msg; sub; if_highlight})

let error ?(loc = none) ?(sub = []) ?(if_highlight = "") msg =
  {loc; msg; sub; if_highlight}

let error_of_exn : (exn -> error option) list ref = ref []

let register_error_of_exn f = error_of_exn := f :: !error_of_exn

let error_of_exn exn =
  let rec loop = function
    | [] -> None
    | f :: rest ->
        match f exn with
        | Some _ as r -> r
        | None -> loop rest
  in
  loop !error_of_exn

let rec default_error_reporter ppf ({loc; msg; sub; if_highlight} as err) =
  let highlighted =
    if if_highlight <> "" then
      let rec collect_locs locs {loc; sub; if_highlight; _} =
        List.fold_left collect_locs (loc :: locs) sub
      in
      let locs = collect_locs [] err in
      highlight_locations ppf locs
    else
      false
  in
  if highlighted then
    Format.pp_print_string ppf if_highlight
  else begin
    print ppf loc;
    Format.pp_print_string ppf msg;
    List.iter (fun err -> Format.fprintf ppf "@\n@[<2>%a@]" default_error_reporter err)
              sub
  end

let error_reporter = ref default_error_reporter

let report_error ppf err =
  print_updating_num_loc_lines ppf !error_reporter err
;;

let error_of_printer loc print x =
  let buf = Buffer.create 64 in
  let ppf = Format.formatter_of_buffer buf in
  pp_print_string ppf "Error: ";
  print ppf x;
  pp_print_flush ppf ();
  let msg = Buffer.contents buf in
  errorf ~loc "%s" msg

let error_of_printer_file print x =
  error_of_printer (in_file !input_name) print x

let () =
  register_error_of_exn
    (function
      | Sys_error msg ->
          Some (errorf ~loc:(in_file !input_name) "Error: I/O error: %s" msg)
      | Warnings.Errors n ->
          Some
            (errorf ~loc:(in_file !input_name)
               "Error: Some fatal warnings were triggered (%d occurrences)" n)
      | _ ->
          None
    )


let rec report_exception_rec n ppf exn =
  try match error_of_exn exn with
  | Some err ->
      fprintf ppf "@[%a@]@." report_error err
  | None -> raise exn
  with exn when n > 0 ->
    report_exception_rec (n-1) ppf exn

let report_exception ppf exn = report_exception_rec 5 ppf exn


exception Error of error

let () =
  register_error_of_exn
    (function
      | Error e -> Some e
      | _ -> None
    )

let raise_errorf ?(loc = none) ?(sub = []) ?(if_highlight = "") =
  Printf.ksprintf (fun msg -> raise (Error ({loc; msg; sub; if_highlight})))

end
module Bs_exception : sig 
#1 "bs_exception.mli"
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

type error =
  | Cmj_not_found of string
  | Bs_cyclic_depends of string  list
  | Bs_duplicated_module of string * string
  | Bs_package_not_found of string                                                        
  | Bs_main_not_exist of string 
  | Bs_invalid_path of string
      
val error : error -> 'a 

end = struct
#1 "bs_exception.ml"
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


type error =
  | Cmj_not_found of string
  | Bs_cyclic_depends of string  list
  | Bs_duplicated_module of string * string
  | Bs_package_not_found of string                            
  | Bs_main_not_exist of string 
  | Bs_invalid_path of string
      
exception Error of error

let error err = raise (Error err)

let report_error ppf = function
  | Cmj_not_found s ->
    Format.fprintf ppf "%s not found, cmj format is generated by BuckleScript" s
  | Bs_cyclic_depends  str
    ->
    Format.fprintf ppf "Cyclic depends : @[%a@]"
      (Format.pp_print_list ~pp_sep:Format.pp_print_space
         Format.pp_print_string)
      str
  | Bs_duplicated_module (a,b)
    ->
    Format.fprintf ppf "The build system does not support two files with same names yet %s, %s" a b
  | Bs_main_not_exist main
    ->
    Format.fprintf ppf "File %s not found " main

  | Bs_package_not_found package
    ->
    Format.fprintf ppf "Package %s not found or %s/lib/ocaml does not exist"
      package package
  | Bs_invalid_path path
    ->  Format.pp_print_string ppf ("Invalid path: " ^ path )
let () =
  Location.register_error_of_exn
    (function
      | Error err
        -> Some (Location.error_of_printer_file report_error err)
      | _ -> None
    )



end
module Clflags : sig 
#1 "clflags.mli"
(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2005 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

val objfiles : string list ref
val ccobjs : string list ref
val dllibs : string list ref
val compile_only : bool ref
val output_name : string option ref
val include_dirs : string list ref
val no_std_include : bool ref
val print_types : bool ref
val make_archive : bool ref
val debug : bool ref
val fast : bool ref
val link_everything : bool ref
val custom_runtime : bool ref
val no_check_prims : bool ref
val bytecode_compatible_32 : bool ref
val output_c_object : bool ref
val output_complete_object : bool ref
val all_ccopts : string list ref
val classic : bool ref
val nopervasives : bool ref
val open_modules : string list ref
val preprocessor : string option ref
val all_ppx : string list ref
val annotations : bool ref
val binary_annotations : bool ref
val use_threads : bool ref
val use_vmthreads : bool ref
val noassert : bool ref
val verbose : bool ref
val noprompt : bool ref
val nopromptcont : bool ref
val init_file : string option ref
val noinit : bool ref
val use_prims : string ref
val use_runtime : string ref
val principal : bool ref
val real_paths : bool ref
val recursive_types : bool ref
val strict_sequence : bool ref
val strict_formats : bool ref
val applicative_functors : bool ref
val make_runtime : bool ref
val gprofile : bool ref
val c_compiler : string option ref
val no_auto_link : bool ref
val dllpaths : string list ref
val make_package : bool ref
val for_package : string option ref
val error_size : int ref
val float_const_prop : bool ref
val transparent_modules : bool ref
val dump_source : bool ref
val dump_parsetree : bool ref
val dump_typedtree : bool ref
val dump_rawlambda : bool ref
val dump_lambda : bool ref
val dump_clambda : bool ref
val dump_instr : bool ref
val keep_asm_file : bool ref
val optimize_for_speed : bool ref
val dump_cmm : bool ref
val dump_selection : bool ref
val dump_cse : bool ref
val dump_live : bool ref
val dump_spill : bool ref
val dump_split : bool ref
val dump_interf : bool ref
val dump_prefer : bool ref
val dump_regalloc : bool ref
val dump_reload : bool ref
val dump_scheduling : bool ref
val dump_linear : bool ref
val keep_startup_file : bool ref
val dump_combine : bool ref
val native_code : bool ref
val inline_threshold : int ref
val dont_write_files : bool ref
val std_include_flag : string -> string
val std_include_dir : unit -> string list
val shared : bool ref
val dlcode : bool ref
val runtime_variant : string ref
val force_slash : bool ref
val keep_docs : bool ref
val keep_locs : bool ref
val unsafe_string : bool ref
val opaque : bool ref

end = struct
#1 "clflags.ml"
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

end
(** Interface as module  *)
module Asttypes
= struct
#1 "asttypes.mli"
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

(* Auxiliary a.s.t. types used by parsetree and typedtree. *)

type constant =
    Const_int of int
  | Const_char of char
  | Const_string of string * string option
  | Const_float of string
  | Const_int32 of int32
  | Const_int64 of int64
  | Const_nativeint of nativeint

type rec_flag = Nonrecursive | Recursive

type direction_flag = Upto | Downto

type private_flag = Private | Public

type mutable_flag = Immutable | Mutable

type virtual_flag = Virtual | Concrete

type override_flag = Override | Fresh

type closed_flag = Closed | Open

type label = string

type 'a loc = 'a Location.loc = {
  txt : 'a;
  loc : Location.t;
}


type variance =
  | Covariant
  | Contravariant
  | Invariant

end
module Misc : sig 
#1 "misc.mli"
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

(* Miscellaneous useful types and functions *)

val fatal_error: string -> 'a
exception Fatal_error

val try_finally : (unit -> 'a) -> (unit -> unit) -> 'a;;

val map_end: ('a -> 'b) -> 'a list -> 'b list -> 'b list
        (* [map_end f l t] is [map f l @ t], just more efficient. *)
val map_left_right: ('a -> 'b) -> 'a list -> 'b list
        (* Like [List.map], with guaranteed left-to-right evaluation order *)
val for_all2: ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
        (* Same as [List.for_all] but for a binary predicate.
           In addition, this [for_all2] never fails: given two lists
           with different lengths, it returns false. *)
val replicate_list: 'a -> int -> 'a list
        (* [replicate_list elem n] is the list with [n] elements
           all identical to [elem]. *)
val list_remove: 'a -> 'a list -> 'a list
        (* [list_remove x l] returns a copy of [l] with the first
           element equal to [x] removed. *)
val split_last: 'a list -> 'a list * 'a
        (* Return the last element and the other elements of the given list. *)
val samelist: ('a -> 'a -> bool) -> 'a list -> 'a list -> bool
        (* Like [List.for_all2] but returns [false] if the two
           lists have different length. *)

val may: ('a -> unit) -> 'a option -> unit
val may_map: ('a -> 'b) -> 'a option -> 'b option

val find_in_path: string list -> string -> string
        (* Search a file in a list of directories. *)
val find_in_path_rel: string list -> string -> string
        (* Search a relative file in a list of directories. *)
val find_in_path_uncap: string list -> string -> string
        (* Same, but search also for uncapitalized name, i.e.
           if name is Foo.ml, allow /path/Foo.ml and /path/foo.ml
           to match. *)
val remove_file: string -> unit
        (* Delete the given file if it exists. Never raise an error. *)
val expand_directory: string -> string -> string
        (* [expand_directory alt file] eventually expands a [+] at the
           beginning of file into [alt] (an alternate root directory) *)

val create_hashtable: int -> ('a * 'b) list -> ('a, 'b) Hashtbl.t
        (* Create a hashtable of the given size and fills it with the
           given bindings. *)

val copy_file: in_channel -> out_channel -> unit
        (* [copy_file ic oc] reads the contents of file [ic] and copies
           them to [oc]. It stops when encountering EOF on [ic]. *)
val copy_file_chunk: in_channel -> out_channel -> int -> unit
        (* [copy_file_chunk ic oc n] reads [n] bytes from [ic] and copies
           them to [oc]. It raises [End_of_file] when encountering
           EOF on [ic]. *)
val string_of_file: in_channel -> string
        (* [string_of_file ic] reads the contents of file [ic] and copies
           them to a string. It stops when encountering EOF on [ic]. *)
val log2: int -> int
        (* [log2 n] returns [s] such that [n = 1 lsl s]
           if [n] is a power of 2*)
val align: int -> int -> int
        (* [align n a] rounds [n] upwards to a multiple of [a]
           (a power of 2). *)
val no_overflow_add: int -> int -> bool
        (* [no_overflow_add n1 n2] returns [true] if the computation of
           [n1 + n2] does not overflow. *)
val no_overflow_sub: int -> int -> bool
        (* [no_overflow_add n1 n2] returns [true] if the computation of
           [n1 - n2] does not overflow. *)
val no_overflow_lsl: int -> bool
        (* [no_overflow_add n] returns [true] if the computation of
           [n lsl 1] does not overflow. *)

val chop_extension_if_any: string -> string
        (* Like Filename.chop_extension but returns the initial file
           name if it has no extension *)

val chop_extensions: string -> string
        (* Return the given file name without its extensions. The extensions
           is the longest suffix starting with a period and not including
           a directory separator, [.xyz.uvw] for instance.

           Return the given name if it does not contain an extension. *)

val search_substring: string -> string -> int -> int
        (* [search_substring pat str start] returns the position of the first
           occurrence of string [pat] in string [str].  Search starts
           at offset [start] in [str].  Raise [Not_found] if [pat]
           does not occur. *)

val replace_substring: before:string -> after:string -> string -> string
        (* [search_substring ~before ~after str] replaces all occurences
           of [before] with [after] in [str] and returns the resulting string. *)

val rev_split_words: string -> string list
        (* [rev_split_words s] splits [s] in blank-separated words, and return
           the list of words in reverse order. *)

val get_ref: 'a list ref -> 'a list
        (* [get_ref lr] returns the content of the list reference [lr] and reset
           its content to the empty list. *)


val fst3: 'a * 'b * 'c -> 'a
val snd3: 'a * 'b * 'c -> 'b
val thd3: 'a * 'b * 'c -> 'c

val fst4: 'a * 'b * 'c * 'd -> 'a
val snd4: 'a * 'b * 'c * 'd -> 'b
val thd4: 'a * 'b * 'c * 'd -> 'c
val for4: 'a * 'b * 'c * 'd -> 'd

module LongString :
  sig
    type t = bytes array
    val create : int -> t
    val length : t -> int
    val get : t -> int -> char
    val set : t -> int -> char -> unit
    val blit : t -> int -> t -> int -> int -> unit
    val output : out_channel -> t -> int -> int -> unit
    val unsafe_blit_to_bytes : t -> int -> bytes -> int -> int -> unit
    val input_bytes : in_channel -> int -> t
  end

val edit_distance : string -> string -> int -> int option
(** [edit_distance a b cutoff] computes the edit distance between
    strings [a] and [b]. To help efficiency, it uses a cutoff: if the
    distance [d] is smaller than [cutoff], it returns [Some d], else
    [None].

    The distance algorithm currently used is Damerau-Levenshtein: it
    computes the number of insertion, deletion, substitution of
    letters, or swapping of adjacent letters to go from one word to the
    other. The particular algorithm may change in the future.
*)

val split : string -> char -> string list
(** [String.split string char] splits the string [string] at every char
    [char], and returns the list of sub-strings between the chars.
    [String.concat (String.make 1 c) (String.split s c)] is the identity.
    @since 4.01
 *)

val cut_at : string -> char -> string * string
(** [String.cut_at s c] returns a pair containing the sub-string before
   the first occurrence of [c] in [s], and the sub-string after the
   first occurrence of [c] in [s].
   [let (before, after) = String.cut_at s c in
    before ^ String.make 1 c ^ after] is the identity if [s] contains [c].

   Raise [Not_found] if the character does not appear in the string
   @since 4.01
*)

end = struct
#1 "misc.ml"
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

(* Errors *)

exception Fatal_error

let fatal_error msg =
  prerr_string ">> Fatal error: "; prerr_endline msg; raise Fatal_error

(* Exceptions *)

let try_finally work cleanup =
  let result = (try work () with e -> cleanup (); raise e) in
  cleanup ();
  result
;;

(* List functions *)

let rec map_end f l1 l2 =
  match l1 with
    [] -> l2
  | hd::tl -> f hd :: map_end f tl l2

let rec map_left_right f = function
    [] -> []
  | hd::tl -> let res = f hd in res :: map_left_right f tl

let rec for_all2 pred l1 l2 =
  match (l1, l2) with
    ([], []) -> true
  | (hd1::tl1, hd2::tl2) -> pred hd1 hd2 && for_all2 pred tl1 tl2
  | (_, _) -> false

let rec replicate_list elem n =
  if n <= 0 then [] else elem :: replicate_list elem (n-1)

let rec list_remove x = function
    [] -> []
  | hd :: tl ->
      if hd = x then tl else hd :: list_remove x tl

let rec split_last = function
    [] -> assert false
  | [x] -> ([], x)
  | hd :: tl ->
      let (lst, last) = split_last tl in
      (hd :: lst, last)

let rec samelist pred l1 l2 =
  match (l1, l2) with
  | ([], []) -> true
  | (hd1 :: tl1, hd2 :: tl2) -> pred hd1 hd2 && samelist pred tl1 tl2
  | (_, _) -> false

(* Options *)

let may f = function
    Some x -> f x
  | None -> ()

let may_map f = function
    Some x -> Some (f x)
  | None -> None

(* File functions *)

let find_in_path path name =
  if not (Filename.is_implicit name) then
    if Sys.file_exists name then name else raise Not_found
  else begin
    let rec try_dir = function
      [] -> raise Not_found
    | dir::rem ->
        let fullname = Filename.concat dir name in
        if Sys.file_exists fullname then fullname else try_dir rem
    in try_dir path
  end

let find_in_path_rel path name =
  let rec simplify s =
    let open Filename in
    let base = basename s in
    let dir = dirname s in
    if dir = s then dir
    else if base = current_dir_name then simplify dir
    else concat (simplify dir) base
  in
  let rec try_dir = function
    [] -> raise Not_found
  | dir::rem ->
      let fullname = simplify (Filename.concat dir name) in
      if Sys.file_exists fullname then fullname else try_dir rem
  in try_dir path

let find_in_path_uncap path name =
  let uname = String.uncapitalize name in
  let rec try_dir = function
    [] -> raise Not_found
  | dir::rem ->
      let fullname = Filename.concat dir name
      and ufullname = Filename.concat dir uname in
      if Sys.file_exists ufullname then ufullname
      else if Sys.file_exists fullname then fullname
      else try_dir rem
  in try_dir path

let remove_file filename =
  try
    Sys.remove filename
  with Sys_error msg ->
    ()

(* Expand a -I option: if it starts with +, make it relative to the standard
   library directory *)

let expand_directory alt s =
  if String.length s > 0 && s.[0] = '+'
  then Filename.concat alt
                       (String.sub s 1 (String.length s - 1))
  else s

(* Hashtable functions *)

let create_hashtable size init =
  let tbl = Hashtbl.create size in
  List.iter (fun (key, data) -> Hashtbl.add tbl key data) init;
  tbl

(* File copy *)

let copy_file ic oc =
  let buff = Bytes.create 0x1000 in
  let rec copy () =
    let n = input ic buff 0 0x1000 in
    if n = 0 then () else (output oc buff 0 n; copy())
  in copy()

let copy_file_chunk ic oc len =
  let buff = Bytes.create 0x1000 in
  let rec copy n =
    if n <= 0 then () else begin
      let r = input ic buff 0 (min n 0x1000) in
      if r = 0 then raise End_of_file else (output oc buff 0 r; copy(n-r))
    end
  in copy len

let string_of_file ic =
  let b = Buffer.create 0x10000 in
  let buff = Bytes.create 0x1000 in
  let rec copy () =
    let n = input ic buff 0 0x1000 in
    if n = 0 then Buffer.contents b else
      (Buffer.add_subbytes b buff 0 n; copy())
  in copy()

(* Integer operations *)

let rec log2 n =
  if n <= 1 then 0 else 1 + log2(n asr 1)

let align n a =
  if n >= 0 then (n + a - 1) land (-a) else n land (-a)

let no_overflow_add a b = (a lxor b) lor (a lxor (lnot (a+b))) < 0

let no_overflow_sub a b = (a lxor (lnot b)) lor (b lxor (a-b)) < 0

let no_overflow_lsl a = min_int asr 1 <= a && a <= max_int asr 1

(* String operations *)

let chop_extension_if_any fname =
  try Filename.chop_extension fname with Invalid_argument _ -> fname

let chop_extensions file =
  let dirname = Filename.dirname file and basename = Filename.basename file in
  try
    let pos = String.index basename '.' in
    let basename = String.sub basename 0 pos in
    if Filename.is_implicit file && dirname = Filename.current_dir_name then
      basename
    else
      Filename.concat dirname basename
  with Not_found -> file

let search_substring pat str start =
  let rec search i j =
    if j >= String.length pat then i
    else if i + j >= String.length str then raise Not_found
    else if str.[i + j] = pat.[j] then search i (j+1)
    else search (i+1) 0
  in search start 0

let replace_substring ~before ~after str =
  let rec search acc curr =
    match search_substring before str curr with
      | next ->
         let prefix = String.sub str curr (next - curr) in
         search (prefix :: acc) (next + String.length before)
      | exception Not_found ->
        let suffix = String.sub str curr (String.length str - curr) in
        List.rev (suffix :: acc)
  in String.concat after (search [] 0)

let rev_split_words s =
  let rec split1 res i =
    if i >= String.length s then res else begin
      match s.[i] with
        ' ' | '\t' | '\r' | '\n' -> split1 res (i+1)
      | _ -> split2 res i (i+1)
    end
  and split2 res i j =
    if j >= String.length s then String.sub s i (j-i) :: res else begin
      match s.[j] with
        ' ' | '\t' | '\r' | '\n' -> split1 (String.sub s i (j-i) :: res) (j+1)
      | _ -> split2 res i (j+1)
    end
  in split1 [] 0

let get_ref r =
  let v = !r in
  r := []; v

let fst3 (x, _, _) = x
let snd3 (_,x,_) = x
let thd3 (_,_,x) = x

let fst4 (x, _, _, _) = x
let snd4 (_,x,_, _) = x
let thd4 (_,_,x,_) = x
let for4 (_,_,_,x) = x


module LongString = struct
  type t = bytes array

  let create str_size =
    let tbl_size = str_size / Sys.max_string_length + 1 in
    let tbl = Array.make tbl_size Bytes.empty in
    for i = 0 to tbl_size - 2 do
      tbl.(i) <- Bytes.create Sys.max_string_length;
    done;
    tbl.(tbl_size - 1) <- Bytes.create (str_size mod Sys.max_string_length);
    tbl

  let length tbl =
    let tbl_size = Array.length tbl in
    Sys.max_string_length * (tbl_size - 1) + Bytes.length tbl.(tbl_size - 1)

  let get tbl ind =
    Bytes.get tbl.(ind / Sys.max_string_length) (ind mod Sys.max_string_length)

  let set tbl ind c =
    Bytes.set tbl.(ind / Sys.max_string_length) (ind mod Sys.max_string_length)
              c

  let blit src srcoff dst dstoff len =
    for i = 0 to len - 1 do
      set dst (dstoff + i) (get src (srcoff + i))
    done

  let output oc tbl pos len =
    for i = pos to pos + len - 1 do
      output_char oc (get tbl i)
    done

  let unsafe_blit_to_bytes src srcoff dst dstoff len =
    for i = 0 to len - 1 do
      Bytes.unsafe_set dst (dstoff + i) (get src (srcoff + i))
    done

  let input_bytes ic len =
    let tbl = create len in
    Array.iter (fun str -> really_input ic str 0 (Bytes.length str)) tbl;
    tbl
end


let edit_distance a b cutoff =
  let la, lb = String.length a, String.length b in
  let cutoff =
    (* using max_int for cutoff would cause overflows in (i + cutoff + 1);
       we bring it back to the (max la lb) worstcase *)
    min (max la lb) cutoff in
  if abs (la - lb) > cutoff then None
  else begin
    (* initialize with 'cutoff + 1' so that not-yet-written-to cases have
       the worst possible cost; this is useful when computing the cost of
       a case just at the boundary of the cutoff diagonal. *)
    let m = Array.make_matrix (la + 1) (lb + 1) (cutoff + 1) in
    m.(0).(0) <- 0;
    for i = 1 to la do
      m.(i).(0) <- i;
    done;
    for j = 1 to lb do
      m.(0).(j) <- j;
    done;
    for i = 1 to la do
      for j = max 1 (i - cutoff - 1) to min lb (i + cutoff + 1) do
        let cost = if a.[i-1] = b.[j-1] then 0 else 1 in
        let best =
          (* insert, delete or substitute *)
          min (1 + min m.(i-1).(j) m.(i).(j-1)) (m.(i-1).(j-1) + cost)
        in
        let best =
          (* swap two adjacent letters; we use "cost" again in case of
             a swap between two identical letters; this is slightly
             redundant as this is a double-substitution case, but it
             was done this way in most online implementations and
             imitation has its virtues *)
          if not (i > 1 && j > 1 && a.[i-1] = b.[j-2] && a.[i-2] = b.[j-1])
          then best
          else min best (m.(i-2).(j-2) + cost)
        in
        m.(i).(j) <- best
      done;
    done;
    let result = m.(la).(lb) in
    if result > cutoff
    then None
    else Some result
  end


(* split a string [s] at every char [c], and return the list of sub-strings *)
let split s c =
  let len = String.length s in
  let rec iter pos to_rev =
    if pos = len then List.rev ("" :: to_rev) else
      match try
              Some ( String.index_from s pos c )
        with Not_found -> None
      with
          Some pos2 ->
            if pos2 = pos then iter (pos+1) ("" :: to_rev) else
              iter (pos2+1) ((String.sub s pos (pos2-pos)) :: to_rev)
        | None -> List.rev ( String.sub s pos (len-pos) :: to_rev )
  in
  iter 0 []

let cut_at s c =
  let pos = String.index s c in
  String.sub s 0 pos, String.sub s (pos+1) (String.length s - pos - 1)

end
module Longident : sig 
#1 "longident.mli"
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

(* Long identifiers, used in parsetree. *)

type t =
    Lident of string
  | Ldot of t * string
  | Lapply of t * t

val flatten: t -> string list
val last: t -> string
val parse: string -> t

end = struct
#1 "longident.ml"
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

type t =
    Lident of string
  | Ldot of t * string
  | Lapply of t * t

let rec flat accu = function
    Lident s -> s :: accu
  | Ldot(lid, s) -> flat (s :: accu) lid
  | Lapply(_, _) -> Misc.fatal_error "Longident.flat"

let flatten lid = flat [] lid

let last = function
    Lident s -> s
  | Ldot(_, s) -> s
  | Lapply(_, _) -> Misc.fatal_error "Longident.last"

let rec split_at_dots s pos =
  try
    let dot = String.index_from s pos '.' in
    String.sub s pos (dot - pos) :: split_at_dots s (dot + 1)
  with Not_found ->
    [String.sub s pos (String.length s - pos)]

let parse s =
  match split_at_dots s 0 with
    [] -> Lident ""  (* should not happen, but don't put assert false
                        so as not to crash the toplevel (see Genprintval) *)
  | hd :: tl -> List.fold_left (fun p s -> Ldot(p, s)) (Lident hd) tl

end
(** Interface as module  *)
module Parsetree
= struct
#1 "parsetree.mli"
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

(** Abstract syntax tree produced by parsing *)

open Asttypes

(** {2 Extension points} *)

type attribute = string loc * payload
       (* [@id ARG]
          [@@id ARG]

          Metadata containers passed around within the AST.
          The compiler ignores unknown attributes.
       *)

and extension = string loc * payload
      (* [%id ARG]
         [%%id ARG]

         Sub-language placeholder -- rejected by the typechecker.
      *)

and attributes = attribute list

and payload =
  | PStr of structure
  | PTyp of core_type  (* : T *)
  | PPat of pattern * expression option  (* ? P  or  ? P when E *)

(** {2 Core language} *)

(* Type expressions *)

and core_type =
    {
     ptyp_desc: core_type_desc;
     ptyp_loc: Location.t;
     ptyp_attributes: attributes; (* ... [@id1] [@id2] *)
    }

and core_type_desc =
  | Ptyp_any
        (*  _ *)
  | Ptyp_var of string
        (* 'a *)
  | Ptyp_arrow of label * core_type * core_type
        (* T1 -> T2       (label = "")
           ~l:T1 -> T2    (label = "l")
           ?l:T1 -> T2    (label = "?l")
         *)
  | Ptyp_tuple of core_type list
        (* T1 * ... * Tn

           Invariant: n >= 2
        *)
  | Ptyp_constr of Longident.t loc * core_type list
        (* tconstr
           T tconstr
           (T1, ..., Tn) tconstr
         *)
  | Ptyp_object of (string * attributes * core_type) list * closed_flag
        (* < l1:T1; ...; ln:Tn >     (flag = Closed)
           < l1:T1; ...; ln:Tn; .. > (flag = Open)
         *)
  | Ptyp_class of Longident.t loc * core_type list
        (* #tconstr
           T #tconstr
           (T1, ..., Tn) #tconstr
         *)
  | Ptyp_alias of core_type * string
        (* T as 'a *)
  | Ptyp_variant of row_field list * closed_flag * label list option
        (* [ `A|`B ]         (flag = Closed; labels = None)
           [> `A|`B ]        (flag = Open;   labels = None)
           [< `A|`B ]        (flag = Closed; labels = Some [])
           [< `A|`B > `X `Y ](flag = Closed; labels = Some ["X";"Y"])
         *)
  | Ptyp_poly of string list * core_type
        (* 'a1 ... 'an. T

           Can only appear in the following context:

           - As the core_type of a Ppat_constraint node corresponding
             to a constraint on a let-binding: let x : 'a1 ... 'an. T
             = e ...

           - Under Cfk_virtual for methods (not values).

           - As the core_type of a Pctf_method node.

           - As the core_type of a Pexp_poly node.

           - As the pld_type field of a label_declaration.

           - As a core_type of a Ptyp_object node.
         *)

  | Ptyp_package of package_type
        (* (module S) *)
  | Ptyp_extension of extension
        (* [%id] *)

and package_type = Longident.t loc * (Longident.t loc * core_type) list
      (*
        (module S)
        (module S with type t1 = T1 and ... and tn = Tn)
       *)

and row_field =
  | Rtag of label * attributes * bool * core_type list
        (* [`A]                   ( true,  [] )
           [`A of T]              ( false, [T] )
           [`A of T1 & .. & Tn]   ( false, [T1;...Tn] )
           [`A of & T1 & .. & Tn] ( true,  [T1;...Tn] )

          - The 2nd field is true if the tag contains a
            constant (empty) constructor.
          - '&' occurs when several types are used for the same constructor
            (see 4.2 in the manual)

          - TODO: switch to a record representation, and keep location
        *)
  | Rinherit of core_type
        (* [ T ] *)

(* Patterns *)

and pattern =
    {
     ppat_desc: pattern_desc;
     ppat_loc: Location.t;
     ppat_attributes: attributes; (* ... [@id1] [@id2] *)
    }

and pattern_desc =
  | Ppat_any
        (* _ *)
  | Ppat_var of string loc
        (* x *)
  | Ppat_alias of pattern * string loc
        (* P as 'a *)
  | Ppat_constant of constant
        (* 1, 'a', "true", 1.0, 1l, 1L, 1n *)
  | Ppat_interval of constant * constant
        (* 'a'..'z'

           Other forms of interval are recognized by the parser
           but rejected by the type-checker. *)
  | Ppat_tuple of pattern list
        (* (P1, ..., Pn)

           Invariant: n >= 2
        *)
  | Ppat_construct of Longident.t loc * pattern option
        (* C                None
           C P              Some P
           C (P1, ..., Pn)  Some (Ppat_tuple [P1; ...; Pn])
         *)
  | Ppat_variant of label * pattern option
        (* `A             (None)
           `A P           (Some P)
         *)
  | Ppat_record of (Longident.t loc * pattern) list * closed_flag
        (* { l1=P1; ...; ln=Pn }     (flag = Closed)
           { l1=P1; ...; ln=Pn; _}   (flag = Open)

           Invariant: n > 0
         *)
  | Ppat_array of pattern list
        (* [| P1; ...; Pn |] *)
  | Ppat_or of pattern * pattern
        (* P1 | P2 *)
  | Ppat_constraint of pattern * core_type
        (* (P : T) *)
  | Ppat_type of Longident.t loc
        (* #tconst *)
  | Ppat_lazy of pattern
        (* lazy P *)
  | Ppat_unpack of string loc
        (* (module P)
           Note: (module P : S) is represented as
           Ppat_constraint(Ppat_unpack, Ptyp_package)
         *)
  | Ppat_exception of pattern
        (* exception P *)
  | Ppat_extension of extension
        (* [%id] *)

(* Value expressions *)

and expression =
    {
     pexp_desc: expression_desc;
     pexp_loc: Location.t;
     pexp_attributes: attributes; (* ... [@id1] [@id2] *)
    }

and expression_desc =
  | Pexp_ident of Longident.t loc
        (* x
           M.x
         *)
  | Pexp_constant of constant
        (* 1, 'a', "true", 1.0, 1l, 1L, 1n *)
  | Pexp_let of rec_flag * value_binding list * expression
        (* let P1 = E1 and ... and Pn = EN in E       (flag = Nonrecursive)
           let rec P1 = E1 and ... and Pn = EN in E   (flag = Recursive)
         *)
  | Pexp_function of case list
        (* function P1 -> E1 | ... | Pn -> En *)
  | Pexp_fun of label * expression option * pattern * expression
        (* fun P -> E1                          (lab = "", None)
           fun ~l:P -> E1                       (lab = "l", None)
           fun ?l:P -> E1                       (lab = "?l", None)
           fun ?l:(P = E0) -> E1                (lab = "?l", Some E0)

           Notes:
           - If E0 is provided, lab must start with '?'.
           - "fun P1 P2 .. Pn -> E1" is represented as nested Pexp_fun.
           - "let f P = E" is represented using Pexp_fun.
         *)
  | Pexp_apply of expression * (label * expression) list
        (* E0 ~l1:E1 ... ~ln:En
           li can be empty (non labeled argument) or start with '?'
           (optional argument).

           Invariant: n > 0
         *)
  | Pexp_match of expression * case list
        (* match E0 with P1 -> E1 | ... | Pn -> En *)
  | Pexp_try of expression * case list
        (* try E0 with P1 -> E1 | ... | Pn -> En *)
  | Pexp_tuple of expression list
        (* (E1, ..., En)

           Invariant: n >= 2
        *)
  | Pexp_construct of Longident.t loc * expression option
        (* C                None
           C E              Some E
           C (E1, ..., En)  Some (Pexp_tuple[E1;...;En])
        *)
  | Pexp_variant of label * expression option
        (* `A             (None)
           `A E           (Some E)
         *)
  | Pexp_record of (Longident.t loc * expression) list * expression option
        (* { l1=P1; ...; ln=Pn }     (None)
           { E0 with l1=P1; ...; ln=Pn }   (Some E0)

           Invariant: n > 0
         *)
  | Pexp_field of expression * Longident.t loc
        (* E.l *)
  | Pexp_setfield of expression * Longident.t loc * expression
        (* E1.l <- E2 *)
  | Pexp_array of expression list
        (* [| E1; ...; En |] *)
  | Pexp_ifthenelse of expression * expression * expression option
        (* if E1 then E2 else E3 *)
  | Pexp_sequence of expression * expression
        (* E1; E2 *)
  | Pexp_while of expression * expression
        (* while E1 do E2 done *)
  | Pexp_for of
      pattern *  expression * expression * direction_flag * expression
        (* for i = E1 to E2 do E3 done      (flag = Upto)
           for i = E1 downto E2 do E3 done  (flag = Downto)
         *)
  | Pexp_constraint of expression * core_type
        (* (E : T) *)
  | Pexp_coerce of expression * core_type option * core_type
        (* (E :> T)        (None, T)
           (E : T0 :> T)   (Some T0, T)
         *)
  | Pexp_send of expression * string
        (*  E # m *)
  | Pexp_new of Longident.t loc
        (* new M.c *)
  | Pexp_setinstvar of string loc * expression
        (* x <- 2 *)
  | Pexp_override of (string loc * expression) list
        (* {< x1 = E1; ...; Xn = En >} *)
  | Pexp_letmodule of string loc * module_expr * expression
        (* let module M = ME in E *)
  | Pexp_assert of expression
        (* assert E
           Note: "assert false" is treated in a special way by the
           type-checker. *)
  | Pexp_lazy of expression
        (* lazy E *)
  | Pexp_poly of expression * core_type option
        (* Used for method bodies.

           Can only be used as the expression under Cfk_concrete
           for methods (not values). *)
  | Pexp_object of class_structure
        (* object ... end *)
  | Pexp_newtype of string * expression
        (* fun (type t) -> E *)
  | Pexp_pack of module_expr
        (* (module ME)

           (module ME : S) is represented as
           Pexp_constraint(Pexp_pack, Ptyp_package S) *)
  | Pexp_open of override_flag * Longident.t loc * expression
        (* let open M in E
           let! open M in E
        *)
  | Pexp_extension of extension
        (* [%id] *)

and case =   (* (P -> E) or (P when E0 -> E) *)
    {
     pc_lhs: pattern;
     pc_guard: expression option;
     pc_rhs: expression;
    }

(* Value descriptions *)

and value_description =
    {
     pval_name: string loc;
     pval_type: core_type;
     pval_prim: string list;
     pval_attributes: attributes;  (* ... [@@id1] [@@id2] *)
     pval_loc: Location.t;
    }

(*
  val x: T                            (prim = [])
  external x: T = "s1" ... "sn"       (prim = ["s1";..."sn"])

  Note: when used under Pstr_primitive, prim cannot be empty
*)

(* Type declarations *)

and type_declaration =
    {
     ptype_name: string loc;
     ptype_params: (core_type * variance) list;
           (* ('a1,...'an) t; None represents  _*)
     ptype_cstrs: (core_type * core_type * Location.t) list;
           (* ... constraint T1=T1'  ... constraint Tn=Tn' *)
     ptype_kind: type_kind;
     ptype_private: private_flag;   (* = private ... *)
     ptype_manifest: core_type option;  (* = T *)
     ptype_attributes: attributes;   (* ... [@@id1] [@@id2] *)
     ptype_loc: Location.t;
    }

(*
  type t                     (abstract, no manifest)
  type t = T0                (abstract, manifest=T0)
  type t = C of T | ...      (variant,  no manifest)
  type t = T0 = C of T | ... (variant,  manifest=T0)
  type t = {l: T; ...}       (record,   no manifest)
  type t = T0 = {l : T; ...} (record,   manifest=T0)
  type t = ..                (open,     no manifest)
*)

and type_kind =
  | Ptype_abstract
  | Ptype_variant of constructor_declaration list
        (* Invariant: non-empty list *)
  | Ptype_record of label_declaration list
        (* Invariant: non-empty list *)
  | Ptype_open

and label_declaration =
    {
     pld_name: string loc;
     pld_mutable: mutable_flag;
     pld_type: core_type;
     pld_loc: Location.t;
     pld_attributes: attributes; (* l [@id1] [@id2] : T *)
    }

(*  { ...; l: T; ... }            (mutable=Immutable)
    { ...; mutable l: T; ... }    (mutable=Mutable)

    Note: T can be a Ptyp_poly.
*)

and constructor_declaration =
    {
     pcd_name: string loc;
     pcd_args: core_type list;
     pcd_res: core_type option;
     pcd_loc: Location.t;
     pcd_attributes: attributes; (* C [@id1] [@id2] of ... *)
    }
(*
  | C of T1 * ... * Tn     (res = None)
  | C: T0                  (args = [], res = Some T0)
  | C: T1 * ... * Tn -> T0 (res = Some T0)
*)

and type_extension =
    {
     ptyext_path: Longident.t loc;
     ptyext_params: (core_type * variance) list;
     ptyext_constructors: extension_constructor list;
     ptyext_private: private_flag;
     ptyext_attributes: attributes;   (* ... [@@id1] [@@id2] *)
    }
(*
  type t += ...
*)

and extension_constructor =
    {
     pext_name: string loc;
     pext_kind : extension_constructor_kind;
     pext_loc : Location.t;
     pext_attributes: attributes; (* C [@id1] [@id2] of ... *)
    }

and extension_constructor_kind =
    Pext_decl of core_type list * core_type option
      (*
         | C of T1 * ... * Tn     ([T1; ...; Tn], None)
         | C: T0                  ([], Some T0)
         | C: T1 * ... * Tn -> T0 ([T1; ...; Tn], Some T0)
       *)
  | Pext_rebind of Longident.t loc
      (*
         | C = D
       *)

(** {2 Class language} *)

(* Type expressions for the class language *)

and class_type =
    {
     pcty_desc: class_type_desc;
     pcty_loc: Location.t;
     pcty_attributes: attributes; (* ... [@id1] [@id2] *)
    }

and class_type_desc =
  | Pcty_constr of Longident.t loc * core_type list
        (* c
           ['a1, ..., 'an] c *)
  | Pcty_signature of class_signature
        (* object ... end *)
  | Pcty_arrow of label * core_type * class_type
        (* T -> CT       (label = "")
           ~l:T -> CT    (label = "l")
           ?l:T -> CT    (label = "?l")
         *)
  | Pcty_extension of extension
        (* [%id] *)

and class_signature =
    {
     pcsig_self: core_type;
     pcsig_fields: class_type_field list;
    }
(* object('selfpat) ... end
   object ... end             (self = Ptyp_any)
 *)

and class_type_field =
    {
     pctf_desc: class_type_field_desc;
     pctf_loc: Location.t;
     pctf_attributes: attributes; (* ... [@@id1] [@@id2] *)
    }

and class_type_field_desc =
  | Pctf_inherit of class_type
        (* inherit CT *)
  | Pctf_val of (string * mutable_flag * virtual_flag * core_type)
        (* val x: T *)
  | Pctf_method  of (string * private_flag * virtual_flag * core_type)
        (* method x: T

           Note: T can be a Ptyp_poly.
         *)
  | Pctf_constraint  of (core_type * core_type)
        (* constraint T1 = T2 *)
  | Pctf_attribute of attribute
        (* [@@@id] *)
  | Pctf_extension of extension
        (* [%%id] *)

and 'a class_infos =
    {
     pci_virt: virtual_flag;
     pci_params: (core_type * variance) list;
     pci_name: string loc;
     pci_expr: 'a;
     pci_loc: Location.t;
     pci_attributes: attributes;  (* ... [@@id1] [@@id2] *)
    }
(* class c = ...
   class ['a1,...,'an] c = ...
   class virtual c = ...

   Also used for "class type" declaration.
*)

and class_description = class_type class_infos

and class_type_declaration = class_type class_infos

(* Value expressions for the class language *)

and class_expr =
    {
     pcl_desc: class_expr_desc;
     pcl_loc: Location.t;
     pcl_attributes: attributes; (* ... [@id1] [@id2] *)
    }

and class_expr_desc =
  | Pcl_constr of Longident.t loc * core_type list
        (* c
           ['a1, ..., 'an] c *)
  | Pcl_structure of class_structure
        (* object ... end *)
  | Pcl_fun of label * expression option * pattern * class_expr
        (* fun P -> CE                          (lab = "", None)
           fun ~l:P -> CE                       (lab = "l", None)
           fun ?l:P -> CE                       (lab = "?l", None)
           fun ?l:(P = E0) -> CE                (lab = "?l", Some E0)
         *)
  | Pcl_apply of class_expr * (label * expression) list
        (* CE ~l1:E1 ... ~ln:En
           li can be empty (non labeled argument) or start with '?'
           (optional argument).

           Invariant: n > 0
         *)
  | Pcl_let of rec_flag * value_binding list * class_expr
        (* let P1 = E1 and ... and Pn = EN in CE      (flag = Nonrecursive)
           let rec P1 = E1 and ... and Pn = EN in CE  (flag = Recursive)
         *)
  | Pcl_constraint of class_expr * class_type
        (* (CE : CT) *)
  | Pcl_extension of extension
        (* [%id] *)

and class_structure =
    {
     pcstr_self: pattern;
     pcstr_fields: class_field list;
    }
(* object(selfpat) ... end
   object ... end           (self = Ppat_any)
 *)

and class_field =
    {
     pcf_desc: class_field_desc;
     pcf_loc: Location.t;
     pcf_attributes: attributes; (* ... [@@id1] [@@id2] *)
    }

and class_field_desc =
  | Pcf_inherit of override_flag * class_expr * string option
        (* inherit CE
           inherit CE as x
           inherit! CE
           inherit! CE as x
         *)
  | Pcf_val of (string loc * mutable_flag * class_field_kind)
        (* val x = E
           val virtual x: T
         *)
  | Pcf_method of (string loc * private_flag * class_field_kind)
        (* method x = E            (E can be a Pexp_poly)
           method virtual x: T     (T can be a Ptyp_poly)
         *)
  | Pcf_constraint of (core_type * core_type)
        (* constraint T1 = T2 *)
  | Pcf_initializer of expression
        (* initializer E *)
  | Pcf_attribute of attribute
        (* [@@@id] *)
  | Pcf_extension of extension
        (* [%%id] *)

and class_field_kind =
  | Cfk_virtual of core_type
  | Cfk_concrete of override_flag * expression

and class_declaration = class_expr class_infos

(** {2 Module language} *)

(* Type expressions for the module language *)

and module_type =
    {
     pmty_desc: module_type_desc;
     pmty_loc: Location.t;
     pmty_attributes: attributes; (* ... [@id1] [@id2] *)
    }

and module_type_desc =
  | Pmty_ident of Longident.t loc
        (* S *)
  | Pmty_signature of signature
        (* sig ... end *)
  | Pmty_functor of string loc * module_type option * module_type
        (* functor(X : MT1) -> MT2 *)
  | Pmty_with of module_type * with_constraint list
        (* MT with ... *)
  | Pmty_typeof of module_expr
        (* module type of ME *)
  | Pmty_extension of extension
        (* [%id] *)
  | Pmty_alias of Longident.t loc
        (* (module M) *)

and signature = signature_item list

and signature_item =
    {
     psig_desc: signature_item_desc;
     psig_loc: Location.t;
    }

and signature_item_desc =
  | Psig_value of value_description
        (*
          val x: T
          external x: T = "s1" ... "sn"
         *)
  | Psig_type of type_declaration list
        (* type t1 = ... and ... and tn = ... *)
  | Psig_typext of type_extension
        (* type t1 += ... *)
  | Psig_exception of extension_constructor
        (* exception C of T *)
  | Psig_module of module_declaration
        (* module X : MT *)
  | Psig_recmodule of module_declaration list
        (* module rec X1 : MT1 and ... and Xn : MTn *)
  | Psig_modtype of module_type_declaration
        (* module type S = MT
           module type S *)
  | Psig_open of open_description
        (* open X *)
  | Psig_include of include_description
        (* include MT *)
  | Psig_class of class_description list
        (* class c1 : ... and ... and cn : ... *)
  | Psig_class_type of class_type_declaration list
        (* class type ct1 = ... and ... and ctn = ... *)
  | Psig_attribute of attribute
        (* [@@@id] *)
  | Psig_extension of extension * attributes
        (* [%%id] *)

and module_declaration =
    {
     pmd_name: string loc;
     pmd_type: module_type;
     pmd_attributes: attributes; (* ... [@@id1] [@@id2] *)
     pmd_loc: Location.t;
    }
(* S : MT *)

and module_type_declaration =
    {
     pmtd_name: string loc;
     pmtd_type: module_type option;
     pmtd_attributes: attributes; (* ... [@@id1] [@@id2] *)
     pmtd_loc: Location.t;
    }
(* S = MT
   S       (abstract module type declaration, pmtd_type = None)
*)

and open_description =
    {
     popen_lid: Longident.t loc;
     popen_override: override_flag;
     popen_loc: Location.t;
     popen_attributes: attributes;
    }
(* open! X - popen_override = Override (silences the 'used identifier
                              shadowing' warning)
   open  X - popen_override = Fresh
 *)

and 'a include_infos =
    {
     pincl_mod: 'a;
     pincl_loc: Location.t;
     pincl_attributes: attributes;
    }

and include_description = module_type include_infos
(* include MT *)

and include_declaration = module_expr include_infos
(* include ME *)

and with_constraint =
  | Pwith_type of Longident.t loc * type_declaration
        (* with type X.t = ...

           Note: the last component of the longident must match
           the name of the type_declaration. *)
  | Pwith_module of Longident.t loc * Longident.t loc
        (* with module X.Y = Z *)
  | Pwith_typesubst of type_declaration
        (* with type t := ... *)
  | Pwith_modsubst of string loc * Longident.t loc
        (* with module X := Z *)

(* Value expressions for the module language *)

and module_expr =
    {
     pmod_desc: module_expr_desc;
     pmod_loc: Location.t;
     pmod_attributes: attributes; (* ... [@id1] [@id2] *)
    }

and module_expr_desc =
  | Pmod_ident of Longident.t loc
        (* X *)
  | Pmod_structure of structure
        (* struct ... end *)
  | Pmod_functor of string loc * module_type option * module_expr
        (* functor(X : MT1) -> ME *)
  | Pmod_apply of module_expr * module_expr
        (* ME1(ME2) *)
  | Pmod_constraint of module_expr * module_type
        (* (ME : MT) *)
  | Pmod_unpack of expression
        (* (val E) *)
  | Pmod_extension of extension
        (* [%id] *)

and structure = structure_item list

and structure_item =
    {
     pstr_desc: structure_item_desc;
     pstr_loc: Location.t;
    }

and structure_item_desc =
  | Pstr_eval of expression * attributes
        (* E *)
  | Pstr_value of rec_flag * value_binding list
        (* let P1 = E1 and ... and Pn = EN       (flag = Nonrecursive)
           let rec P1 = E1 and ... and Pn = EN   (flag = Recursive)
         *)
  | Pstr_primitive of value_description
        (* external x: T = "s1" ... "sn" *)
  | Pstr_type of type_declaration list
        (* type t1 = ... and ... and tn = ... *)
  | Pstr_typext of type_extension
        (* type t1 += ... *)
  | Pstr_exception of extension_constructor
        (* exception C of T
           exception C = M.X *)
  | Pstr_module of module_binding
        (* module X = ME *)
  | Pstr_recmodule of module_binding list
        (* module rec X1 = ME1 and ... and Xn = MEn *)
  | Pstr_modtype of module_type_declaration
        (* module type S = MT *)
  | Pstr_open of open_description
        (* open X *)
  | Pstr_class of class_declaration list
        (* class c1 = ... and ... and cn = ... *)
  | Pstr_class_type of class_type_declaration list
        (* class type ct1 = ... and ... and ctn = ... *)
  | Pstr_include of include_declaration
        (* include ME *)
  | Pstr_attribute of attribute
        (* [@@@id] *)
  | Pstr_extension of extension * attributes
        (* [%%id] *)

and value_binding =
  {
    pvb_pat: pattern;
    pvb_expr: expression;
    pvb_attributes: attributes;
    pvb_loc: Location.t;
  }

and module_binding =
    {
     pmb_name: string loc;
     pmb_expr: module_expr;
     pmb_attributes: attributes;
     pmb_loc: Location.t;
    }
(* X = ME *)

(** {2 Toplevel} *)

(* Toplevel phrases *)

type toplevel_phrase =
  | Ptop_def of structure
  | Ptop_dir of string * directive_argument
     (* #use, #load ... *)

and directive_argument =
  | Pdir_none
  | Pdir_string of string
  | Pdir_int of int
  | Pdir_ident of Longident.t
  | Pdir_bool of bool

end
module Depend : sig 
#1 "depend.mli"
(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(** Module dependencies. *)

module StringSet : Set.S with type elt = string

val free_structure_names : StringSet.t ref

val open_module : StringSet.t -> Longident.t -> unit

val add_use_file : StringSet.t -> Parsetree.toplevel_phrase list -> unit

val add_signature : StringSet.t -> Parsetree.signature -> unit

val add_implementation : StringSet.t -> Parsetree.structure -> unit

end = struct
#1 "depend.ml"
(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open Asttypes
open Location
open Longident
open Parsetree

module StringSet = Set.Make(struct type t = string let compare = compare end)

(* Collect free module identifiers in the a.s.t. *)

let free_structure_names = ref StringSet.empty

let rec add_path bv = function
  | Lident s ->
      if not (StringSet.mem s bv)
      then free_structure_names := StringSet.add s !free_structure_names
  | Ldot(l, _s) -> add_path bv l
  | Lapply(l1, l2) -> add_path bv l1; add_path bv l2

let open_module bv lid = add_path bv lid

let add bv lid =
  match lid.txt with
    Ldot(l, _s) -> add_path bv l
  | _ -> ()

let addmodule bv lid = add_path bv lid.txt

let rec add_type bv ty =
  match ty.ptyp_desc with
    Ptyp_any -> ()
  | Ptyp_var _ -> ()
  | Ptyp_arrow(_, t1, t2) -> add_type bv t1; add_type bv t2
  | Ptyp_tuple tl -> List.iter (add_type bv) tl
  | Ptyp_constr(c, tl) -> add bv c; List.iter (add_type bv) tl
  | Ptyp_object (fl, _) -> List.iter (fun (_, _, t) -> add_type bv t) fl
  | Ptyp_class(c, tl) -> add bv c; List.iter (add_type bv) tl
  | Ptyp_alias(t, _) -> add_type bv t
  | Ptyp_variant(fl, _, _) ->
      List.iter
        (function Rtag(_,_,_,stl) -> List.iter (add_type bv) stl
          | Rinherit sty -> add_type bv sty)
        fl
  | Ptyp_poly(_, t) -> add_type bv t
  | Ptyp_package pt -> add_package_type bv pt
  | Ptyp_extension _ -> ()

and add_package_type bv (lid, l) =
  add bv lid;
  List.iter (add_type bv) (List.map (fun (_, e) -> e) l)

let add_opt add_fn bv = function
    None -> ()
  | Some x -> add_fn bv x

let add_constructor_decl bv pcd =
  List.iter (add_type bv) pcd.pcd_args; Misc.may (add_type bv) pcd.pcd_res

let add_type_declaration bv td =
  List.iter
    (fun (ty1, ty2, _) -> add_type bv ty1; add_type bv ty2)
    td.ptype_cstrs;
  add_opt add_type bv td.ptype_manifest;
  let add_tkind = function
    Ptype_abstract -> ()
  | Ptype_variant cstrs ->
      List.iter (add_constructor_decl bv) cstrs
  | Ptype_record lbls ->
      List.iter (fun pld -> add_type bv pld.pld_type) lbls
  | Ptype_open -> () in
  add_tkind td.ptype_kind

let add_extension_constructor bv ext =
  match ext.pext_kind with
      Pext_decl(args, rty) ->
        List.iter (add_type bv) args; Misc.may (add_type bv) rty
    | Pext_rebind lid -> add bv lid

let add_type_extension bv te =
  add bv te.ptyext_path;
  List.iter (add_extension_constructor bv) te.ptyext_constructors

let rec add_class_type bv cty =
  match cty.pcty_desc with
    Pcty_constr(l, tyl) ->
      add bv l; List.iter (add_type bv) tyl
  | Pcty_signature { pcsig_self = ty; pcsig_fields = fieldl } ->
      add_type bv ty;
      List.iter (add_class_type_field bv) fieldl
  | Pcty_arrow(_, ty1, cty2) ->
      add_type bv ty1; add_class_type bv cty2
  | Pcty_extension _ -> ()

and add_class_type_field bv pctf =
  match pctf.pctf_desc with
    Pctf_inherit cty -> add_class_type bv cty
  | Pctf_val(_, _, _, ty) -> add_type bv ty
  | Pctf_method(_, _, _, ty) -> add_type bv ty
  | Pctf_constraint(ty1, ty2) -> add_type bv ty1; add_type bv ty2
  | Pctf_attribute _ -> ()
  | Pctf_extension _ -> ()

let add_class_description bv infos =
  add_class_type bv infos.pci_expr

let add_class_type_declaration = add_class_description

let pattern_bv = ref StringSet.empty

let rec add_pattern bv pat =
  match pat.ppat_desc with
    Ppat_any -> ()
  | Ppat_var _ -> ()
  | Ppat_alias(p, _) -> add_pattern bv p
  | Ppat_interval _
  | Ppat_constant _ -> ()
  | Ppat_tuple pl -> List.iter (add_pattern bv) pl
  | Ppat_construct(c, op) -> add bv c; add_opt add_pattern bv op
  | Ppat_record(pl, _) ->
      List.iter (fun (lbl, p) -> add bv lbl; add_pattern bv p) pl
  | Ppat_array pl -> List.iter (add_pattern bv) pl
  | Ppat_or(p1, p2) -> add_pattern bv p1; add_pattern bv p2
  | Ppat_constraint(p, ty) -> add_pattern bv p; add_type bv ty
  | Ppat_variant(_, op) -> add_opt add_pattern bv op
  | Ppat_type li -> add bv li
  | Ppat_lazy p -> add_pattern bv p
  | Ppat_unpack id -> pattern_bv := StringSet.add id.txt !pattern_bv
  | Ppat_exception p -> add_pattern bv p
  | Ppat_extension _ -> ()

let add_pattern bv pat =
  pattern_bv := bv;
  add_pattern bv pat;
  !pattern_bv

let rec add_expr bv exp =
  match exp.pexp_desc with
    Pexp_ident l -> add bv l
  | Pexp_constant _ -> ()
  | Pexp_let(rf, pel, e) ->
      let bv = add_bindings rf bv pel in add_expr bv e
  | Pexp_fun (_, opte, p, e) ->
      add_opt add_expr bv opte; add_expr (add_pattern bv p) e
  | Pexp_function pel ->
      add_cases bv pel
  | Pexp_apply(e, el) ->
      add_expr bv e; List.iter (fun (_,e) -> add_expr bv e) el
  | Pexp_match(e, pel) -> add_expr bv e; add_cases bv pel
  | Pexp_try(e, pel) -> add_expr bv e; add_cases bv pel
  | Pexp_tuple el -> List.iter (add_expr bv) el
  | Pexp_construct(c, opte) -> add bv c; add_opt add_expr bv opte
  | Pexp_variant(_, opte) -> add_opt add_expr bv opte
  | Pexp_record(lblel, opte) ->
      List.iter (fun (lbl, e) -> add bv lbl; add_expr bv e) lblel;
      add_opt add_expr bv opte
  | Pexp_field(e, fld) -> add_expr bv e; add bv fld
  | Pexp_setfield(e1, fld, e2) -> add_expr bv e1; add bv fld; add_expr bv e2
  | Pexp_array el -> List.iter (add_expr bv) el
  | Pexp_ifthenelse(e1, e2, opte3) ->
      add_expr bv e1; add_expr bv e2; add_opt add_expr bv opte3
  | Pexp_sequence(e1, e2) -> add_expr bv e1; add_expr bv e2
  | Pexp_while(e1, e2) -> add_expr bv e1; add_expr bv e2
  | Pexp_for( _, e1, e2, _, e3) ->
      add_expr bv e1; add_expr bv e2; add_expr bv e3
  | Pexp_coerce(e1, oty2, ty3) ->
      add_expr bv e1;
      add_opt add_type bv oty2;
      add_type bv ty3
  | Pexp_constraint(e1, ty2) ->
      add_expr bv e1;
      add_type bv ty2
  | Pexp_send(e, _m) -> add_expr bv e
  | Pexp_new li -> add bv li
  | Pexp_setinstvar(_v, e) -> add_expr bv e
  | Pexp_override sel -> List.iter (fun (_s, e) -> add_expr bv e) sel
  | Pexp_letmodule(id, m, e) ->
      add_module bv m; add_expr (StringSet.add id.txt bv) e
  | Pexp_assert (e) -> add_expr bv e
  | Pexp_lazy (e) -> add_expr bv e
  | Pexp_poly (e, t) -> add_expr bv e; add_opt add_type bv t
  | Pexp_object { pcstr_self = pat; pcstr_fields = fieldl } ->
      let bv = add_pattern bv pat in List.iter (add_class_field bv) fieldl
  | Pexp_newtype (_, e) -> add_expr bv e
  | Pexp_pack m -> add_module bv m
  | Pexp_open (_ovf, m, e) -> open_module bv m.txt; add_expr bv e
  | Pexp_extension _ -> ()

and add_cases bv cases =
  List.iter (add_case bv) cases

and add_case bv {pc_lhs; pc_guard; pc_rhs} =
  let bv = add_pattern bv pc_lhs in
  add_opt add_expr bv pc_guard;
  add_expr bv pc_rhs

and add_bindings recf bv pel =
  let bv' = List.fold_left (fun bv x -> add_pattern bv x.pvb_pat) bv pel in
  let bv = if recf = Recursive then bv' else bv in
  List.iter (fun x -> add_expr bv x.pvb_expr) pel;
  bv'

and add_modtype bv mty =
  match mty.pmty_desc with
    Pmty_ident l -> add bv l
  | Pmty_alias l -> addmodule bv l
  | Pmty_signature s -> add_signature bv s
  | Pmty_functor(id, mty1, mty2) ->
      Misc.may (add_modtype bv) mty1;
      add_modtype (StringSet.add id.txt bv) mty2
  | Pmty_with(mty, cstrl) ->
      add_modtype bv mty;
      List.iter
        (function
          | Pwith_type (_, td) -> add_type_declaration bv td
          | Pwith_module (_, lid) -> addmodule bv lid
          | Pwith_typesubst td -> add_type_declaration bv td
          | Pwith_modsubst (_, lid) -> addmodule bv lid
        )
        cstrl
  | Pmty_typeof m -> add_module bv m
  | Pmty_extension _ -> ()

and add_signature bv = function
    [] -> ()
  | item :: rem -> add_signature (add_sig_item bv item) rem

and add_sig_item bv item =
  match item.psig_desc with
    Psig_value vd ->
      add_type bv vd.pval_type; bv
  | Psig_type dcls ->
      List.iter (add_type_declaration bv) dcls; bv
  | Psig_typext te ->
      add_type_extension bv te; bv
  | Psig_exception pext ->
      add_extension_constructor bv pext; bv
  | Psig_module pmd ->
      add_modtype bv pmd.pmd_type; StringSet.add pmd.pmd_name.txt bv
  | Psig_recmodule decls ->
      let bv' =
        List.fold_right StringSet.add
                        (List.map (fun pmd -> pmd.pmd_name.txt) decls) bv
      in
      List.iter (fun pmd -> add_modtype bv' pmd.pmd_type) decls;
      bv'
  | Psig_modtype x ->
      begin match x.pmtd_type with
        None -> ()
      | Some mty -> add_modtype bv mty
      end;
      bv
  | Psig_open od ->
      open_module bv od.popen_lid.txt; bv
  | Psig_include incl ->
      add_modtype bv incl.pincl_mod; bv
  | Psig_class cdl ->
      List.iter (add_class_description bv) cdl; bv
  | Psig_class_type cdtl ->
      List.iter (add_class_type_declaration bv) cdtl; bv
  | Psig_attribute _ | Psig_extension _ ->
      bv

and add_module bv modl =
  match modl.pmod_desc with
    Pmod_ident l -> addmodule bv l
  | Pmod_structure s -> ignore (add_structure bv s)
  | Pmod_functor(id, mty, modl) ->
      Misc.may (add_modtype bv) mty;
      add_module (StringSet.add id.txt bv) modl
  | Pmod_apply(mod1, mod2) ->
      add_module bv mod1; add_module bv mod2
  | Pmod_constraint(modl, mty) ->
      add_module bv modl; add_modtype bv mty
  | Pmod_unpack(e) ->
      add_expr bv e
  | Pmod_extension _ ->
      ()

and add_structure bv item_list =
  List.fold_left add_struct_item bv item_list

and add_struct_item bv item =
  match item.pstr_desc with
    Pstr_eval (e, _attrs) ->
      add_expr bv e; bv
  | Pstr_value(rf, pel) ->
      let bv = add_bindings rf bv pel in bv
  | Pstr_primitive vd ->
      add_type bv vd.pval_type; bv
  | Pstr_type dcls ->
      List.iter (add_type_declaration bv) dcls; bv
  | Pstr_typext te ->
      add_type_extension bv te;
      bv
  | Pstr_exception pext ->
      add_extension_constructor bv pext; bv
  | Pstr_module x ->
      add_module bv x.pmb_expr; StringSet.add x.pmb_name.txt bv
  | Pstr_recmodule bindings ->
      let bv' =
        List.fold_right StringSet.add
          (List.map (fun x -> x.pmb_name.txt) bindings) bv in
      List.iter
        (fun x -> add_module bv' x.pmb_expr)
        bindings;
      bv'
  | Pstr_modtype x ->
      begin match x.pmtd_type with
        None -> ()
      | Some mty -> add_modtype bv mty
      end;
      bv
  | Pstr_open od ->
      open_module bv od.popen_lid.txt; bv
  | Pstr_class cdl ->
      List.iter (add_class_declaration bv) cdl; bv
  | Pstr_class_type cdtl ->
      List.iter (add_class_type_declaration bv) cdtl; bv
  | Pstr_include incl ->
      add_module bv incl.pincl_mod; bv
  | Pstr_attribute _ | Pstr_extension _ ->
      bv

and add_use_file bv top_phrs =
  ignore (List.fold_left add_top_phrase bv top_phrs)

and add_implementation bv l =
  ignore (add_structure bv l)

and add_top_phrase bv = function
  | Ptop_def str -> add_structure bv str
  | Ptop_dir (_, _) -> bv

and add_class_expr bv ce =
  match ce.pcl_desc with
    Pcl_constr(l, tyl) ->
      add bv l; List.iter (add_type bv) tyl
  | Pcl_structure { pcstr_self = pat; pcstr_fields = fieldl } ->
      let bv = add_pattern bv pat in List.iter (add_class_field bv) fieldl
  | Pcl_fun(_, opte, pat, ce) ->
      add_opt add_expr bv opte;
      let bv = add_pattern bv pat in add_class_expr bv ce
  | Pcl_apply(ce, exprl) ->
      add_class_expr bv ce; List.iter (fun (_,e) -> add_expr bv e) exprl
  | Pcl_let(rf, pel, ce) ->
      let bv = add_bindings rf bv pel in add_class_expr bv ce
  | Pcl_constraint(ce, ct) ->
      add_class_expr bv ce; add_class_type bv ct
  | Pcl_extension _ -> ()

and add_class_field bv pcf =
  match pcf.pcf_desc with
    Pcf_inherit(_, ce, _) -> add_class_expr bv ce
  | Pcf_val(_, _, Cfk_concrete (_, e))
  | Pcf_method(_, _, Cfk_concrete (_, e)) -> add_expr bv e
  | Pcf_val(_, _, Cfk_virtual ty)
  | Pcf_method(_, _, Cfk_virtual ty) -> add_type bv ty
  | Pcf_constraint(ty1, ty2) -> add_type bv ty1; add_type bv ty2
  | Pcf_initializer e -> add_expr bv e
  | Pcf_attribute _ | Pcf_extension _ -> ()

and add_class_declaration bv decl =
  add_class_expr bv decl.pci_expr

end
module Ext_pervasives : sig 
#1 "ext_pervasives.mli"
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








(** Extension to standard library [Pervavives] module, safe to open 
  *)

external reraise: exn -> 'a = "%reraise"

val finally : 'a -> ('a -> 'c) -> ('a -> 'b) -> 'b

val with_file_as_chan : string -> (out_channel -> 'a) -> 'a

val with_file_as_pp : string -> (Format.formatter -> 'a) -> 'a

val is_pos_pow : Int32.t -> int

val failwithf : loc:string -> ('a, unit, string, 'b) format4 -> 'a

val invalid_argf : ('a, unit, string, 'b) format4 -> 'a

val bad_argf : ('a, unit, string, 'b) format4 -> 'a



val dump : 'a -> string 

external id : 'a -> 'a = "%identity"

(** Copied from {!Btype.hash_variant}:
    need sync up and add test case
 *)
val hash_variant : string -> int

end = struct
#1 "ext_pervasives.ml"
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






external reraise: exn -> 'a = "%reraise"

let finally v action f   = 
  match f v with
  | exception e -> 
      action v ;
      reraise e 
  | e ->  action v ; e 

let with_file_as_chan filename f = 
  finally (open_out filename) close_out f 

let with_file_as_pp filename f = 
  finally (open_out filename) close_out
    (fun chan -> 
      let fmt = Format.formatter_of_out_channel chan in
      let v = f  fmt in
      Format.pp_print_flush fmt ();
      v
    ) 


let  is_pos_pow n = 
  let module M = struct exception E end in 
  let rec aux c (n : Int32.t) = 
    if n <= 0l then -2 
    else if n = 1l then c 
    else if Int32.logand n 1l =  0l then   
      aux (c + 1) (Int32.shift_right n 1 )
    else raise M.E in 
  try aux 0 n  with M.E -> -1

let failwithf ~loc fmt = Format.ksprintf (fun s -> failwith (loc ^ s))
    fmt
    
let invalid_argf fmt = Format.ksprintf invalid_arg fmt

let bad_argf fmt = Format.ksprintf (fun x -> raise (Arg.Bad x ) ) fmt


let rec dump r =
  if Obj.is_int r then
    string_of_int (Obj.magic r : int)
  else (* Block. *)
    let rec get_fields acc = function
      | 0 -> acc
      | n -> let n = n-1 in get_fields (Obj.field r n :: acc) n
    in
    let rec is_list r =
      if Obj.is_int r then
        r = Obj.repr 0 (* [] *)
      else
        let s = Obj.size r and t = Obj.tag r in
        t = 0 && s = 2 && is_list (Obj.field r 1) (* h :: t *)
    in
    let rec get_list r =
      if Obj.is_int r then
        []
      else
        let h = Obj.field r 0 and t = get_list (Obj.field r 1) in
        h :: t
    in
    let opaque name =
      (* XXX In future, print the address of value 'r'.  Not possible
       * in pure OCaml at the moment.  *)
      "<" ^ name ^ ">"
    in
    let s = Obj.size r and t = Obj.tag r in
    (* From the tag, determine the type of block. *)
    match t with
    | _ when is_list r ->
      let fields = get_list r in
      "[" ^ String.concat "; " (List.map dump fields) ^ "]"
    | 0 ->
      let fields = get_fields [] s in
      "(" ^ String.concat ", " (List.map dump fields) ^ ")"
    | x when x = Obj.lazy_tag ->
      (* Note that [lazy_tag .. forward_tag] are < no_scan_tag.  Not
         * clear if very large constructed values could have the same
         * tag. XXX *)
      opaque "lazy"
    | x when x = Obj.closure_tag ->
      opaque "closure"
    | x when x = Obj.object_tag ->
      let fields = get_fields [] s in
      let _clasz, id, slots =
        match fields with
        | h::h'::t -> h, h', t
        | _ -> assert false
      in
      (* No information on decoding the class (first field).  So just print
         * out the ID and the slots. *)
      "Object #" ^ dump id ^ " (" ^ String.concat ", " (List.map dump slots) ^ ")"
    | x when x = Obj.infix_tag ->
      opaque "infix"
    | x when x = Obj.forward_tag ->
      opaque "forward"
    | x when x < Obj.no_scan_tag ->
      let fields = get_fields [] s in
      "Tag" ^ string_of_int t ^
      " (" ^ String.concat ", " (List.map dump fields) ^ ")"
    | x when x = Obj.string_tag ->
      "\"" ^ String.escaped (Obj.magic r : string) ^ "\""
    | x when x = Obj.double_tag ->
      string_of_float (Obj.magic r : float)
    | x when x = Obj.abstract_tag ->
      opaque "abstract"
    | x when x = Obj.custom_tag ->
      opaque "custom"
    | x when x = Obj.custom_tag ->
      opaque "final"
    | x when x = Obj.double_array_tag ->
      "[|"^
      String.concat ";"
        (Array.to_list (Array.map string_of_float (Obj.magic r : float array))) ^
      "|]"
    | _ ->
      opaque (Printf.sprintf "unknown: tag %d size %d" t s)

let dump v = dump (Obj.repr v)

external id : 'a -> 'a = "%identity"


let hash_variant s =
  let accu = ref 0 in
  for i = 0 to String.length s - 1 do
    accu := 223 * !accu + Char.code s.[i]
  done;
  (* reduce to 31 bits *)
  accu := !accu land (1 lsl 31 - 1);
  (* make it signed for 64 bits architectures *)
  if !accu > 0x3FFFFFFF then !accu - (1 lsl 31) else !accu


end
module Ext_bytes : sig 
#1 "ext_bytes.mli"
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







(** Port the {!Bytes.escaped} from trunk to make it not locale sensitive *)

val escaped : bytes -> bytes

end = struct
#1 "ext_bytes.ml"
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








external char_code: char -> int = "%identity"
external char_chr: int -> char = "%identity"

let escaped s =
  let n = ref 0 in
  for i = 0 to Bytes.length s - 1 do
    n := !n +
      (match Bytes.unsafe_get s i with
       | '"' | '\\' | '\n' | '\t' | '\r' | '\b' -> 2
       | ' ' .. '~' -> 1
       | _ -> 4)
  done;
  if !n = Bytes.length s then Bytes.copy s else begin
    let s' = Bytes.create !n in
    n := 0;
    for i = 0 to Bytes.length s - 1 do
      begin match Bytes.unsafe_get s i with
      | ('"' | '\\') as c ->
          Bytes.unsafe_set s' !n '\\'; incr n; Bytes.unsafe_set s' !n c
      | '\n' ->
          Bytes.unsafe_set s' !n '\\'; incr n; Bytes.unsafe_set s' !n 'n'
      | '\t' ->
          Bytes.unsafe_set s' !n '\\'; incr n; Bytes.unsafe_set s' !n 't'
      | '\r' ->
          Bytes.unsafe_set s' !n '\\'; incr n; Bytes.unsafe_set s' !n 'r'
      | '\b' ->
          Bytes.unsafe_set s' !n '\\'; incr n; Bytes.unsafe_set s' !n 'b'
      | (' ' .. '~') as c -> Bytes.unsafe_set s' !n c
      | c ->
          let a = char_code c in
          Bytes.unsafe_set s' !n '\\';
          incr n;
          Bytes.unsafe_set s' !n (char_chr (48 + a / 100));
          incr n;
          Bytes.unsafe_set s' !n (char_chr (48 + (a / 10) mod 10));
          incr n;
          Bytes.unsafe_set s' !n (char_chr (48 + a mod 10));
      end;
      incr n
    done;
    s'
  end

end
module Ext_string : sig 
#1 "ext_string.mli"
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








(** Extension to the standard library [String] module, avoid locale sensitivity *) 


val trim : string -> string 

val split_by : ?keep_empty:bool -> (char -> bool) -> string -> string list
(** default is false *)

val split : ?keep_empty:bool -> string -> char -> string list
(** default is false *)

val starts_with : string -> string -> bool

(**
   return [-1] when not found, the returned index is useful 
   see [ends_with_then_chop]
*)
val ends_with_index : string -> string -> int

val ends_with : string -> string -> bool

(**
   {[
     ends_with_then_chop "a.cmj" ".cmj"
     "a"
   ]}
   This is useful in controlled or file case sensitve system
*)
val ends_with_then_chop : string -> string -> string option


val escaped : string -> string

val for_all : (char -> bool) -> string -> bool

val is_empty : string -> bool

val repeat : int -> string -> string 

val equal : string -> string -> bool

val find : ?start:int -> sub:string -> string -> int

val rfind : sub:string -> string -> int

val tail_from : string -> int -> string

val digits_of_str : string -> offset:int -> int -> int

val starts_with_and_number : string -> offset:int -> string -> int

end = struct
#1 "ext_string.ml"
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








let split_by ?(keep_empty=false) is_delim str =
  let len = String.length str in
  let rec loop acc last_pos pos =
    if pos = -1 then
      if last_pos = 0 && not keep_empty then
        (*
           {[ split " test_unsafe_obj_ffi_ppx.cmi" ~keep_empty:false ' ']}
        *)
        acc
      else 
        String.sub str 0 last_pos :: acc
    else
      if is_delim str.[pos] then
        let new_len = (last_pos - pos - 1) in
        if new_len <> 0 || keep_empty then 
          let v = String.sub str (pos + 1) new_len in
          loop ( v :: acc)
            pos (pos - 1)
        else loop acc pos (pos - 1)
    else loop acc last_pos (pos - 1)
  in
  loop [] len (len - 1)

let trim s = 
  let i = ref 0  in
  let j = String.length s in 
  while !i < j &&  let u = s.[!i] in u = '\t' || u = '\n' || u = ' ' do 
    incr i;
  done;
  let k = ref (j - 1)  in 
  while !k >= !i && let u = s.[!k] in u = '\t' || u = '\n' || u = ' ' do 
    decr k ;
  done;
  String.sub s !i (!k - !i + 1)

let split ?keep_empty  str on = 
  if str = "" then [] else 
  split_by ?keep_empty (fun x -> (x : char) = on) str  ;;

let starts_with s beg = 
  let beg_len = String.length beg in
  let s_len = String.length s in
   beg_len <=  s_len &&
  (let i = ref 0 in
    while !i <  beg_len 
          && String.unsafe_get s !i =
             String.unsafe_get beg !i do 
      incr i 
    done;
    !i = beg_len
  )



let ends_with_index s beg = 
  let s_finish = String.length s - 1 in
  let s_beg = String.length beg - 1 in
  if s_beg > s_finish then -1
  else
    let rec aux j k = 
      if k < 0 then (j + 1)
      else if String.unsafe_get s j = String.unsafe_get beg k then 
        aux (j - 1) (k - 1)
      else  -1 in 
    aux s_finish s_beg

let ends_with s beg = ends_with_index s beg >= 0 


let ends_with_then_chop s beg = 
  let i =  ends_with_index s beg in 
  if i >= 0 then Some (String.sub s 0 i) 
  else None

(**  In OCaml 4.02.3, {!String.escaped} is locale senstive, 
     this version try to make it not locale senstive, this bug is fixed
     in the compiler trunk     
*)
let escaped s =
  let rec needs_escape i =
    if i >= String.length s then false else
      match String.unsafe_get s i with
      | '"' | '\\' | '\n' | '\t' | '\r' | '\b' -> true
      | ' ' .. '~' -> needs_escape (i+1)
      | _ -> true
  in
  if needs_escape 0 then
    Bytes.unsafe_to_string (Ext_bytes.escaped (Bytes.unsafe_of_string s))
  else
    s


let for_all (p : char -> bool) s = 
  let len = String.length s in
  let rec aux i = 
    if i >= len then true 
    else  p (String.unsafe_get s i) && aux (i + 1)
  in aux 0 

let is_empty s = String.length s = 0


let repeat n s  =
  let len = String.length s in
  let res = Bytes.create(n * len) in
  for i = 0 to pred n do
    String.blit s 0 res (i * len) len
  done;
  Bytes.to_string res

let equal (x : string) y  = x = y



let _is_sub ~sub i s j ~len =
  let rec check k =
    if k = len
    then true
    else 
      String.unsafe_get sub (i+k) = 
      String.unsafe_get s (j+k) && check (k+1)
  in
  j+len <= String.length s && check 0



let find ?(start=0) ~sub s =
  let n = String.length sub in
  let i = ref start in
  let module M = struct exception Exit end  in
  try
    while !i + n <= String.length s do
      if _is_sub ~sub 0 s !i ~len:n then raise M.Exit;
      incr i
    done;
    -1
  with M.Exit ->
    !i


let rfind ~sub s =
  let n = String.length sub in
  let i = ref (String.length s - n) in
  let module M = struct exception Exit end in 
  try
    while !i >= 0 do
      if _is_sub ~sub 0 s !i ~len:n then raise M.Exit;
      decr i
    done;
    -1
  with M.Exit ->
    !i

let tail_from s x = 
  let len = String.length s  in 
  if  x > len then invalid_arg ("Ext_string.tail_from " ^s ^ " : "^ string_of_int x )
  else String.sub s x (len - x)


(**
   {[ 
     digits_of_str "11_js" 2 == 11     
   ]}
*)
let digits_of_str s ~offset x = 
  let rec aux i acc s x  = 
    if i >= x then acc 
    else aux (i + 1) (10 * acc + Char.code s.[offset + i] - 48 (* Char.code '0' *)) s x in 
  aux 0 0 s x 



(*
   {[
     starts_with_and_number "js_fn_mk_01" 0 "js_fn_mk_" = 1 ;;
     starts_with_and_number "js_fn_run_02" 0 "js_fn_mk_" = -1 ;;
     starts_with_and_number "js_fn_mk_03" 6 "mk_" = 3 ;;
     starts_with_and_number "js_fn_mk_04" 6 "run_" = -1;;
     starts_with_and_number "js_fn_run_04" 6 "run_" = 4;;
     (starts_with_and_number "js_fn_run_04" 6 "run_" = 3) = false ;;
   ]}
*)
let starts_with_and_number s ~offset beg =
  let beg_len = String.length beg in
  let s_len = String.length s in
  let finish_delim = offset + beg_len in 

   if finish_delim >  s_len  then -1 
   else 
     let i = ref offset  in
      while !i <  finish_delim
            && String.unsafe_get s !i =
               String.unsafe_get beg (!i - offset) do 
        incr i 
      done;
      if !i = finish_delim then 
        digits_of_str ~offset:finish_delim s 2 
      else 
        -1 

let equal (x : string) y  = x = y

end
module Literals : sig 
#1 "literals.mli"
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






val js_array_ctor : string 
val js_type_number : string
val js_type_string : string
val js_type_object : string
val js_undefined : string
val js_prop_length : string

val param : string
val partial_arg : string
val prim : string

(**temporary varaible used in {!Js_ast_util} *)
val tmp : string 

val create : string 

val app : string
val app_array : string

val runtime : string
val stdlib : string
val imul : string

val setter_suffix : string
val setter_suffix_len : int


val js_debugger : string
val js_pure_expr : string
val js_pure_stmt : string
val js_unsafe_downgrade : string
val js_fn_run : string
val js_method_run : string
val js_fn_method : string
val js_fn_mk : string

(** callback actually, not exposed to user yet *)
val js_fn_runmethod : string 

val bs_deriving : string
val bs_deriving_dot : string
val bs_type : string

(** nodejs *)

val node_modules : string
val node_modules_length : int
val package_json : string  

val suffix_cmj : string
val suffix_cmi : string
val suffix_ml : string
val suffix_mlast : string 
val suffix_mliast : string
val suffix_mll : string
val suffix_d : string
val suffix_mlastd : string
val suffix_mliastd : string

end = struct
#1 "literals.ml"
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







let js_array_ctor = "Array"
let js_type_number = "number"
let js_type_string = "string"
let js_type_object = "object" 
let js_undefined = "undefined"
let js_prop_length = "length"

let prim = "prim"
let param = "param"
let partial_arg = "partial_arg"
let tmp = "tmp"

let create = "create" (* {!Caml_exceptions.create}*)

let app = "_"
let app_array = "app" (* arguments are an array*)

let runtime = "runtime" (* runtime directory *)

let stdlib = "stdlib"

let imul = "imul" (* signed int32 mul *)

let setter_suffix = "#="
let setter_suffix_len = String.length setter_suffix

let js_debugger = "js_debugger"
let js_pure_expr = "js_pure_expr"
let js_pure_stmt = "js_pure_stmt"
let js_unsafe_downgrade = "js_unsafe_downgrade"
let js_fn_run = "js_fn_run"
let js_method_run = "js_method_run"

let js_fn_method = "js_fn_method"
let js_fn_mk = "js_fn_mk"
let js_fn_runmethod = "js_fn_runmethod"

let bs_deriving = "bs.deriving"
let bs_deriving_dot = "bs.deriving."
let bs_type = "bs.type"


(** nodejs *)
let node_modules = "node_modules"
let node_modules_length = String.length "node_modules"
let package_json = "package.json"


let suffix_cmj = ".cmj"
let suffix_cmi = ".cmi"
let suffix_mll = ".mll"
let suffix_ml = ".ml"
let suffix_mlast = ".mlast"
let suffix_mliast = ".mliast"
let suffix_d = ".d"
let suffix_mlastd = ".mlast.d"
let suffix_mliastd = ".mliast.d"



end
module Ext_filename : sig 
#1 "ext_filename.mli"
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





(* TODO:
   Change the module name, this code is not really an extension of the standard 
    library but rather specific to JS Module name convention. 
*)

type t = 
  [ `File of string 
  | `Dir of string ]

val combine : string -> string -> string 
val path_as_directory : string -> string

(** An extension module to calculate relative path follow node/npm style. 
    TODO : this short name will have to change upon renaming the file.
 *)

(** Js_output is node style, which means 
    separator is only '/'

    if the path contains 'node_modules', 
    [node_relative_path] will discard its prefix and 
    just treat it as a library instead
 *)

val node_relative_path : t -> [`File of string] -> string

val chop_extension : ?loc:string -> string -> string






val cwd : string Lazy.t
val package_dir : string Lazy.t

val replace_backward_slash : string -> string

val module_name_of_file : string -> string

val chop_extension_if_any : string -> string

val absolute_path : string -> string

end = struct
#1 "ext_filename.ml"
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








(** Used when produce node compatible paths *)
let node_sep = "/"
let node_parent = ".."
let node_current = "."

type t = 
  [ `File of string 
  | `Dir of string ]

let cwd = lazy (Sys.getcwd ())

let (//) = Filename.concat 

let combine path1 path2 =
  if path1 = "" then
    path2
  else if path2 = "" then path1
  else 
  if Filename.is_relative path2 then
     path1// path2 
  else
    path2

(* Note that [.//] is the same as [./] *)
let path_as_directory x =
  if x = "" then x
  else
  if Ext_string.ends_with x  Filename.dir_sep then
    x 
  else 
    x ^ Filename.dir_sep

let absolute_path s = 
  let process s = 
    let s = 
      if Filename.is_relative s then
        Lazy.force cwd // s 
      else s in
    (* Now simplify . and .. components *)
    let rec aux s =
      let base,dir  = Filename.basename s, Filename.dirname s  in
      if dir = s then dir
      else if base = Filename.current_dir_name then aux dir
      else if base = Filename.parent_dir_name then Filename.dirname (aux dir)
      else aux dir // base
    in aux s  in 
  process s 


let chop_extension ?(loc="") name =
  try Filename.chop_extension name 
  with Invalid_argument _ -> 
    Ext_pervasives.invalid_argf 
      "Filename.chop_extension ( %s : %s )"  loc name

let chop_extension_if_any fname =
  try Filename.chop_extension fname with Invalid_argument _ -> fname



(** example
    {[
    "/bb/mbigc/mbig2899/bgit/bucklescript/jscomp/stdlib/external/pervasives.cmj"
    "/bb/mbigc/mbig2899/bgit/bucklescript/jscomp/stdlib/ocaml_array.ml"
    ]}

    The other way
    {[
    
    "/bb/mbigc/mbig2899/bgit/bucklescript/jscomp/stdlib/ocaml_array.ml"
    "/bb/mbigc/mbig2899/bgit/bucklescript/jscomp/stdlib/external/pervasives.cmj"
    ]}
    {[
    "/bb/mbigc/mbig2899/bgit/bucklescript/jscomp/stdlib//ocaml_array.ml"
    ]}
    {[
    /a/b
    /c/d
    ]}
 *)
let relative_path file_or_dir_1 file_or_dir_2 = 
  let sep_char = Filename.dir_sep.[0] in
  let relevant_dir1 = 
    (match file_or_dir_1 with 
    | `Dir x -> x 
    | `File file1 ->  Filename.dirname file1) in
  let relevant_dir2 = 
    (match file_or_dir_2 with 
    |`Dir x -> x 
    |`File file2 -> Filename.dirname file2 ) in
  let dir1 = Ext_string.split relevant_dir1 sep_char   in
  let dir2 = Ext_string.split relevant_dir2 sep_char  in
  let rec go (dir1 : string list) (dir2 : string list) = 
    match dir1, dir2 with 
    | x::xs , y :: ys when x = y
      -> go xs ys 
    | _, _
      -> 
        List.map (fun _ -> node_parent) dir2 @ dir1 
  in
  match go dir1 dir2 with
  | (x :: _ ) as ys when x = node_parent -> 
      String.concat node_sep ys
  | ys -> 
      String.concat node_sep  @@ node_current :: ys





let os_path_separator_char = String.unsafe_get Filename.dir_sep 0 

(** path2: a/b 
    path1: a 
    result:  ./b 
    TODO: [Filename.concat] with care

    [file1] is currently compilation file 
    [file2] is the dependency
 *)
let node_relative_path (file1 : t) 
    (`File file2 as dep_file : [`File of string]) = 
  let v = Ext_string.find  file2 ~sub:Literals.node_modules in 
  let len = String.length file2 in 
  if v >= 0 then
    let rec skip  i =       
      if i >= len then
        Ext_pervasives.failwithf ~loc:__LOC__ "invalid path: %s"  file2
      else 
        (* https://en.wikipedia.org/wiki/Path_(computing))
           most path separator are a single char 
        *)
        let curr_char = String.unsafe_get file2 i  in 
        if curr_char = os_path_separator_char || curr_char = '.' then 
          skip (i + 1) 
        else i
        (*
          TODO: we need do more than this suppose user 
          input can be
           {[
           "xxxghsoghos/ghsoghso/node_modules/../buckle-stdlib/list.js"
           ]}
           This seems weird though
        *)
    in 
    Ext_string.tail_from file2
      (skip (v + Literals.node_modules_length)) 
  else 
    relative_path 
      (  match dep_file with 
         | `File x -> `File (absolute_path x)
         | `Dir x -> `Dir (absolute_path x))

       (match file1 with 
         | `File x -> `File (absolute_path x)
         | `Dir x -> `Dir(absolute_path x))
     ^ node_sep ^
    chop_extension_if_any (Filename.basename file2)





let find_package_json_dir cwd  = 
  let rec aux cwd  = 
    if Sys.file_exists (cwd // Literals.package_json) then cwd
    else 
      let cwd' = Filename.dirname cwd in 
      if String.length cwd' < String.length cwd then  
        aux cwd'
      else 
        Ext_pervasives.failwithf 
          ~loc:__LOC__
            "package.json not found from %s" cwd
  in
  aux cwd 

let package_dir = lazy (find_package_json_dir (Lazy.force cwd))

let replace_backward_slash (x : string)= 
  String.map (function 
    |'\\'-> '/'
    | x -> x) x  

let module_name_of_file file =
    String.capitalize 
      (Filename.chop_extension @@ Filename.basename file)  

(** For win32 or case insensitve OS 
    [".cmj"] is the same as [".CMJ"]
  *)
(* let has_exact_suffix_then_chop fname suf =  *)
  

end
module Ext_format : sig 
#1 "ext_format.mli"
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








(** Simplified wrapper module for the standard library [Format] module. 
  *) 

type t = private Format.formatter

val string : t -> string -> unit

val break : t -> unit

val break1 : t -> unit

val space :  t -> unit

val group : t -> int -> (unit -> 'a) -> 'a
(** [group] will record current indentation 
    and indent futher
 *)

val vgroup : t -> int -> (unit -> 'a) -> 'a

val paren : t -> (unit -> 'a) -> 'a

val paren_group : t -> int -> (unit -> 'a) -> 'a

val brace_group : t -> int -> (unit -> 'a) -> 'a

val brace_vgroup : t -> int -> (unit -> 'a) -> 'a

val bracket_group : t -> int -> (unit -> 'a) -> 'a

val newline : t -> unit

val to_out_channel : out_channel -> t

val flush : t -> unit -> unit

val pp_print_queue :
  ?pp_sep:(Format.formatter -> unit -> unit) ->
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a Queue.t -> unit

end = struct
#1 "ext_format.ml"
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








open Format

type t = formatter

let string = pp_print_string

let break = fun fmt -> pp_print_break fmt 0 0

let break1 =
  fun fmt -> pp_print_break fmt 0 1 

let space  fmt  = 
  pp_print_break fmt 1 0

let vgroup fmt indent u = 
  pp_open_vbox fmt indent; 
  let v = u () in
  pp_close_box fmt ();
  v

let group fmt indent u = 
  pp_open_hovbox fmt indent; 
  let v = u () in
  pp_close_box fmt ();
  v
  
let paren fmt u = 
  string fmt "(";
  let v = u () in
  string fmt ")";
  v

let brace fmt u = 
  string fmt "{";
  (* break1 fmt ; *)
  let v = u () in
  string fmt "}";
  v 

let bracket fmt u = 
  string fmt "[";
  let v = u () in
  string fmt "]";
  v 

let paren_group st n action = 
  group st n (fun _ -> paren st action)

let brace_group st n action = 
  group st n (fun _ -> brace st action )

let brace_vgroup st n action = 
  vgroup st n (fun _ -> 
    string st "{";
    pp_print_break st 0 2;
    let v = vgroup st 0 action in
    pp_print_break st 0 0;
    string st "}";
    v
              )
let bracket_group st n action = 
  group st n (fun _ -> bracket st action)

let newline fmt = pp_print_newline fmt ()

let to_out_channel = formatter_of_out_channel

(* let non_breaking_space  fmt = string fmt " " *)
(* let set_needed_space_function _ _ = () *)
let flush = pp_print_flush

let list = pp_print_list

let rec pp_print_queue ?(pp_sep = pp_print_cut) pp_v ppf q =
  Queue.iter (fun q -> pp_v ppf q ;  pp_sep ppf ()) q 

end
module Ext_list : sig 
#1 "ext_list.mli"
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








(** Extension to the standard library [List] module *)
    
(** TODO some function are no efficiently implemented. *) 

val filter_map : ('a -> 'b option) -> 'a list -> 'b list 

val excludes : ('a -> bool) -> 'a list -> bool * 'a list
val exclude_with_fact : ('a -> bool) -> 'a list -> 'a option * 'a list
val exclude_with_fact2 : 
  ('a -> bool) -> ('a -> bool) -> 'a list -> 'a option * 'a option * 'a list
val same_length : 'a list -> 'b list -> bool

val init : int -> (int -> 'a) -> 'a list

val take : int -> 'a list -> 'a list * 'a list
val try_take : int -> 'a list -> 'a list * int * 'a list 

val exclude_tail : 'a list -> 'a * 'a list

val filter_map2 : ('a -> 'b -> 'c option) -> 'a list -> 'b list -> 'c list

val filter_map2i : (int -> 'a -> 'b -> 'c option) -> 'a list -> 'b list -> 'c list

val filter_mapi : (int -> 'a -> 'b option) -> 'a list -> 'b list

val flat_map2 : ('a -> 'b -> 'c list) -> 'a list -> 'b list -> 'c list

val flat_map : ('a -> 'b list) -> 'a list -> 'b list 

(** for the last element the first element will be passed [true] *)

val fold_right2_last : (bool -> 'a -> 'b -> 'c -> 'c) -> 'a list -> 'b list -> 'c -> 'c

val map_last : (bool -> 'a -> 'b) -> 'a list -> 'b list

val stable_group : ('a -> 'a -> bool) -> 'a list -> 'a list list

val drop : int -> 'a list -> 'a list 

val for_all_ret : ('a -> bool) -> 'a list -> 'a option

val for_all_opt : ('a -> 'b option) -> 'a list -> 'b option
(** [for_all_opt f l] returns [None] if all return [None],  
    otherwise returns the first one. 
 *)

val fold : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
(** same as [List.fold_left]. 
    Provide an api so that list can be easily swapped by other containers  
 *)

val rev_map_append : ('a -> 'b) -> 'a list -> 'b list -> 'b list

val rev_map_acc : 'a list -> ('b -> 'a) -> 'b list -> 'a list

val rev_iter : ('a -> unit) -> 'a list -> unit

val for_all2_no_exn : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool

val find_opt : ('a -> 'b option) -> 'a list -> 'b option

(** [f] is applied follow the list order *)
val split_map : ('a -> 'b * 'c) -> 'a list -> 'b list * 'c list       


val reduce_from_right : ('a -> 'a -> 'a) -> 'a list -> 'a

(** [fn] is applied from left to right *)
val reduce_from_left : ('a -> 'a -> 'a) -> 'a list -> 'a


type 'a t = 'a list ref

val create_ref_empty : unit -> 'a t

val ref_top : 'a t -> 'a 

val ref_empty : 'a t -> bool

val ref_push : 'a -> 'a t -> unit

val ref_pop : 'a t -> 'a

val rev_except_last : 'a list -> 'a list * 'a

val sort_via_array :
  ('a -> 'a -> int) -> 'a list -> 'a list

end = struct
#1 "ext_list.ml"
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








let rec filter_map (f: 'a -> 'b option) xs = 
  match xs with 
  | [] -> []
  | y :: ys -> 
      begin match f y with 
      | None -> filter_map f ys
      | Some z -> z :: filter_map f ys
      end

let excludes (p : 'a -> bool ) l : bool * 'a list=
  let excluded = ref false in 
  let rec aux accu = function
  | [] -> List.rev accu
  | x :: l -> 
    if p x then 
      begin 
        excluded := true ;
        aux accu l
      end
    else aux (x :: accu) l in
  let v = aux [] l in 
  if !excluded then true, v else false,l

let exclude_with_fact p l =
  let excluded = ref None in 
  let rec aux accu = function
  | [] -> List.rev accu
  | x :: l -> 
    if p x then 
      begin 
        excluded := Some x ;
        aux accu l
      end
    else aux (x :: accu) l in
  let v = aux [] l in 
  !excluded , if !excluded <> None then v else l 


(** Make sure [p2 x] and [p1 x] will not hold at the same time *)
let exclude_with_fact2 p1 p2 l =
  let excluded1 = ref None in 
  let excluded2 = ref None in 
  let rec aux accu = function
  | [] -> List.rev accu
  | x :: l -> 
    if p1 x then 
      begin 
        excluded1 := Some x ;
        aux accu l
      end
    else if p2 x then 
      begin 
        excluded2 := Some x ; 
        aux accu l 
      end
    else aux (x :: accu) l in
  let v = aux [] l in 
  !excluded1, !excluded2 , if !excluded1 <> None && !excluded2 <> None then v else l 



let rec same_length xs ys = 
  match xs, ys with 
  | [], [] -> true
  | _::xs, _::ys -> same_length xs ys 
  | _, _ -> false 

let  filter_mapi (f: int -> 'a -> 'b option) xs = 
  let rec aux i xs = 
    match xs with 
    | [] -> []
    | y :: ys -> 
        begin match f i y with 
        | None -> aux (i + 1) ys
        | Some z -> z :: aux (i + 1) ys
        end in
  aux 0 xs 

let rec filter_map2 (f: 'a -> 'b -> 'c option) xs ys = 
  match xs,ys with 
  | [],[] -> []
  | u::us, v :: vs -> 
      begin match f u v with 
      | None -> filter_map2 f us vs (* idea: rec f us vs instead? *)
      | Some z -> z :: filter_map2 f us vs
      end
  | _ -> invalid_arg "Ext_list.filter_map2"

let filter_map2i (f: int ->  'a -> 'b -> 'c option) xs ys = 
  let rec aux i xs ys = 
  match xs,ys with 
  | [],[] -> []
  | u::us, v :: vs -> 
      begin match f i u v with 
      | None -> aux (i + 1) us vs (* idea: rec f us vs instead? *)
      | Some z -> z :: aux (i + 1) us vs
      end
  | _ -> invalid_arg "Ext_list.filter_map2i" in
  aux 0 xs ys

let rec rev_map_append  f l1 l2 =
  match l1 with
  | [] -> l2
  | a :: l -> rev_map_append f l (f a :: l2)

let flat_map2 f lx ly = 
  let rec aux acc lx ly = 
    match lx, ly with 
    | [], [] 
      -> List.rev acc
    | x::xs, y::ys 
      ->  aux (List.rev_append (f x y) acc) xs ys
    | _, _ -> invalid_arg "Ext_list.flat_map2" in
  aux [] lx ly
        
let flat_map f lx =
  let rec aux acc lx =
    match lx with
    | [] -> List.rev acc
    | y::ys -> aux (List.rev_append ( f y)  acc ) ys in
  aux [] lx

let rec map2_last f l1 l2 =
  match (l1, l2) with
  | ([], []) -> []
  | [u], [v] -> [f true u v ]
  | (a1::l1, a2::l2) -> let r = f false  a1 a2 in r :: map2_last f l1 l2
  | (_, _) -> invalid_arg "List.map2_last"

let rec map_last f l1 =
  match l1 with
  | [] -> []
  | [u]-> [f true u ]
  | a1::l1 -> let r = f false  a1 in r :: map_last f l1


let rec fold_right2_last f l1 l2 accu  = 
  match (l1, l2) with
  | ([], []) -> accu
  | [last1], [last2] -> f true  last1 last2 accu
  | (a1::l1, a2::l2) -> f false a1 a2 (fold_right2_last f l1 l2 accu)
  | (_, _) -> invalid_arg "List.fold_right2"


let init n f = 
  Array.to_list (Array.init n f)

let take n l = 
  let arr = Array.of_list l in 
  let arr_length =  Array.length arr in
  if arr_length  < n then invalid_arg "Ext_list.take"
  else (Array.to_list (Array.sub arr 0 n ), 
        Array.to_list (Array.sub arr n (arr_length - n)))

let try_take n l = 
  let arr = Array.of_list l in 
  let arr_length =  Array.length arr in
  if arr_length  <= n then 
    l,  arr_length, []
  else Array.to_list (Array.sub arr 0 n ), n, (Array.to_list (Array.sub arr n (arr_length - n)))

let exclude_tail (x : 'a list) = 
  let rec aux acc x = 
    match x with 
    | [] -> invalid_arg "Ext_list.exclude_tail"
    | [ x ] ->  x, List.rev acc
    | y0::ys -> aux (y0::acc) ys in
  aux [] x

(* For small list, only need partial equality 
   {[
   group (=) [1;2;3;4;3]
   ;;
   - : int list list = [[3; 3]; [4]; [2]; [1]]
   # group (=) [];;
   - : 'a list list = []
   ]}
 *)
let rec group (cmp : 'a -> 'a -> bool) (lst : 'a list) : 'a list list =
  match lst with 
  | [] -> []
  | x::xs -> 
      aux cmp x (group cmp xs )

and aux cmp (x : 'a)  (xss : 'a list list) : 'a list list = 
  match xss with 
  | [] -> [[x]]
  | y::ys -> 
      if cmp x (List.hd y) (* cannot be null*) then
        (x::y) :: ys 
      else
        y :: aux cmp x ys                                 
  
let stable_group cmp lst =  group cmp lst |> List.rev 

let rec drop n h = 
  if n < 0 then invalid_arg "Ext_list.drop"
  else if n = 0 then h 
  else if h = [] then invalid_arg "Ext_list.drop"
  else 
    drop (n - 1) (List.tl h)

let rec for_all_ret  p = function
  | [] -> None
  | a::l -> 
      if p a 
      then for_all_ret p l
      else Some a 

let rec for_all_opt  p = function
  | [] -> None
  | a::l -> 
      match p a with
      | None -> for_all_opt p l
      | v -> v 

let fold f l init = 
  List.fold_left (fun acc i -> f  i init) init l 

let rev_map_acc  acc f l = 
  let rec rmap_f accu = function
    | [] -> accu
    | a::l -> rmap_f (f a :: accu) l
  in
  rmap_f acc l

let rec rev_iter f xs =
    match xs with    
    | [] -> ()
    | y :: ys -> 
      rev_iter f ys ;
      f y      
      
let rec for_all2_no_exn p l1 l2 = 
  match (l1, l2) with
  | ([], []) -> true
  | (a1::l1, a2::l2) -> p a1 a2 && for_all2_no_exn p l1 l2
  | (_, _) -> false


let rec find_no_exn p = function
  | [] -> None
  | x :: l -> if p x then Some x else find_no_exn p l


let rec find_opt p = function
  | [] -> None
  | x :: l -> 
    match  p x with 
    | Some _ as v  ->  v
    | None -> find_opt p l


let split_map 
    ( f : 'a -> ('b * 'c)) (xs : 'a list ) : 'b list  * 'c list = 
  let rec aux bs cs xs =
    match xs with 
    | [] -> List.rev bs, List.rev cs 
    | u::us -> 
      let b,c =  f u in aux (b::bs) (c ::cs) us in 

  aux [] [] xs 


(*
   {[
     reduce_from_right (-) [1;2;3];;
     - : int = 2
               # reduce_from_right (-) [1;2;3; 4];;
     - : int = -2
                # reduce_from_right (-) [1];;
     - : int = 1
               # reduce_from_right (-) [1;2;3; 4; 5];;
     - : int = 3
   ]} 
*)
let reduce_from_right fn lst = 
  begin match List.rev lst with
    | last :: rest -> 
      List.fold_left  (fun x y -> fn y x) last rest 
    | _ -> invalid_arg "Ext_list.reduce" 
  end
let reduce_from_left fn lst = 
  match lst with 
  | first :: rest ->  List.fold_left fn first rest 
  | _ -> invalid_arg "Ext_list.reduce_from_left"


type 'a t = 'a list ref

let create_ref_empty () = ref []

let ref_top x = 
  match !x with 
  | y::_ -> y 
  | _ -> invalid_arg "Ext_list.ref_top"

let ref_empty x = 
  match !x with [] -> true | _ -> false 

let ref_push x refs = 
  refs := x :: !refs

let ref_pop refs = 
  match !refs with 
  | [] -> invalid_arg "Ext_list.ref_pop"
  | x::rest -> 
    refs := rest ; 
    x     

let rev_except_last xs =
  let rec aux acc xs =
    match xs with
    | [ ] -> invalid_arg "Ext_list.rev_except_last"
    | [ x ] -> acc ,x
    | x :: xs -> aux (x::acc) xs in
  aux [] xs   

let sort_via_array cmp lst =
  let arr = Array.of_list lst  in
  Array.sort cmp arr;
  Array.to_list arr

end
module Js_config : sig 
#1 "js_config.mli"
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


type module_system = 
  [ `NodeJS | `AmdJS | `Goog ] (* This will be serliazed *)


type package_info = 
 (module_system * string )

type package_name  = string
type packages_info =
  | Empty 
  | NonBrowser of (package_name * package_info  list)



val cmj_ext : string 


(* val is_browser : unit -> bool  *)
(* val set_browser : unit -> unit *)


val get_ext : unit -> string

(** depends on [package_infos], used in {!Js_program_loader} *)
val get_output_dir : module_system -> string -> string


(** used by command line option *)
val set_npm_package_path : string -> unit 
val get_packages_info : unit -> packages_info

type info_query = 
  [ `Empty 
  | `Package_script of string
  | `Found of package_name * string
  | `NotFound 
  ]

val query_package_infos : 
  packages_info ->
  module_system ->
  info_query



(** set/get header *)
val no_version_header : bool ref 


(** return [package_name] and [path] 
    when in script mode: 
*)

val get_current_package_name_and_path : 
  module_system -> info_query


val set_package_name : string -> unit 
val get_package_name : unit -> string option

(** corss module inline option *)
val cross_module_inline : bool ref
val set_cross_module_inline : bool -> unit
val get_cross_module_inline : unit -> bool
  
(** diagnose option *)
val diagnose : bool ref 
val get_diagnose : unit -> bool 
val set_diagnose : bool -> unit 


(** generate tds option *)
val default_gen_tds : bool ref

(** options for builtion ppx *)
val no_builtin_ppx_ml : bool ref 
val no_builtin_ppx_mli : bool ref 
val no_warn_ffi_type : bool ref 
val no_warn_unused_bs_attribute : bool ref 

(** check-div-by-zero option *)
val check_div_by_zero : bool ref 
val get_check_div_by_zero : unit -> bool 

(* It will imply [-noassert] be set too, note from the implmentation point of view, 
   in the lambda layer, it is impossible to tell whehther it is [assert (3 <> 2)] or 
   [if (3<>2) then assert false]
 *)
val no_any_assert : bool ref 
val set_no_any_assert : unit -> unit
val get_no_any_assert : unit -> bool 


val block : string
val int32 : string
val gc : string 
val backtrace : string
val version : string
val builtin_exceptions : string
val exceptions : string
val io : string
val oo : string
val sys : string
val lexer : string 
val parser : string
val obj_runtime : string
val array : string
val format : string
val string : string
val bytes : string  
val float : string 
val curry : string 
(* val bigarray : string *)
(* val unix : string *)
val int64 : string
val md5 : string
val hash : string
val weak : string
val js_primitive : string
val module_ : string

(** Debugging utilies *)
val set_current_file : string -> unit 
val get_current_file : unit -> string
val get_module_name : unit -> string

val iset_debug_file : string -> unit
val set_debug_file : string -> unit
val get_debug_file : unit -> string

val is_same_file : unit -> bool 

val tool_name : string

val is_windows : bool 

val better_errors : bool ref
val sort_imports : bool ref 
val dump_js : bool ref
val syntax_only  : bool ref
val binary_ast : bool ref

end = struct
#1 "js_config.ml"
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







type env =
  | NodeJS
  | AmdJS
  | Goog (* of string option *)



type path = string
type module_system =
  [ `NodeJS | `AmdJS | `Goog ]
type package_info =
 ( module_system * string )

type package_name  = string
type packages_info =
  | Empty (* No set *)
  | NonBrowser of (package_name * package_info  list)
(** we don't force people to use package *)



let ext = ref ".js"
let cmj_ext = ".cmj"



let get_ext () = !ext


let packages_info : packages_info ref = ref Empty


let get_package_name () =
  match !packages_info with
  | Empty  -> None
  | NonBrowser(n,_) -> Some n

let no_version_header = ref false

let set_package_name name =
  match !packages_info with
  | Empty -> packages_info := NonBrowser(name,  [])
  |  _ ->
    Ext_pervasives.bad_argf "duplicated flag for -bs-package-name"


let set_npm_package_path s =
  match !packages_info  with
  | Empty ->
    Ext_pervasives.bad_argf "please set package name first using -bs-package-name ";
  | NonBrowser(name,  envs) ->
    let env, path =
      match Ext_string.split ~keep_empty:false s ':' with
      | [ package_name; path]  ->
        (match package_name with
         | "commonjs" -> `NodeJS
         | "amdjs" -> `AmdJS
         | "goog" -> `Goog
         | _ ->
           Ext_pervasives.bad_argf "invalid module system %s" package_name), path
      | [path] ->
        `NodeJS, path
      | _ ->
        Ext_pervasives.bad_argf "invalid npm package path: %s" s
    in
    packages_info := NonBrowser (name,  ((env,path) :: envs))
   (** Browser is not set via command line only for internal use *)




let cross_module_inline = ref false

let get_cross_module_inline () = !cross_module_inline
let set_cross_module_inline b =
  cross_module_inline := b


let diagnose = ref false
let get_diagnose () = !diagnose
let set_diagnose b = diagnose := b

let (//) = Filename.concat

let get_packages_info () = !packages_info

type info_query =
  [ `Empty
  | `Package_script of string
  | `Found of package_name * string
  | `NotFound ]
let query_package_infos package_infos module_system =
  match package_infos with
  | Empty -> `Empty
  | NonBrowser (name, []) -> `Package_script name
  | NonBrowser (name, paths) ->
    begin match List.find (fun (k, _) -> k = module_system) paths with
      | (_, x) -> `Found (name, x)
      | exception _ -> `NotFound
    end

let get_current_package_name_and_path   module_system =
  query_package_infos !packages_info module_system


(* for a single pass compilation, [output_dir]
   can be cached
*)
let get_output_dir module_system filename =
  match !packages_info with
  | Empty | NonBrowser (_, [])->
    if Filename.is_relative filename then
      Lazy.force Ext_filename.cwd //
      Filename.dirname filename
    else
      Filename.dirname filename
  | NonBrowser (_,  modules) ->
    begin match List.find (fun (k,_) -> k = module_system) modules with
      | (_, _path) -> Lazy.force Ext_filename.package_dir // _path
      |  exception _ -> assert false
    end




let default_gen_tds = ref false

let no_builtin_ppx_ml = ref false
let no_builtin_ppx_mli = ref false
let no_warn_ffi_type = ref false

(** TODO: will flip the option when it is ready *)
let no_warn_unused_bs_attribute = ref false


let builtin_exceptions = "Caml_builtin_exceptions"
let exceptions = "Caml_exceptions"
let io = "Caml_io"
let sys = "Caml_sys"
let lexer = "Caml_lexer"
let parser = "Caml_parser"
let obj_runtime = "Caml_obj"
let array = "Caml_array"
let format = "Caml_format"
let string = "Caml_string"
let bytes = "Caml_bytes"
let float = "Caml_float"
let hash = "Caml_hash"
let oo = "Caml_oo"
let curry = "Curry"
let int64 = "Caml_int64"
let md5 = "Caml_md5"
let weak = "Caml_weak"
let backtrace = "Caml_backtrace"
let gc = "Caml_gc"
let int32 = "Caml_int32"
let block = "Block"
let js_primitive = "Js_primitive"
let module_ = "Caml_module"
let version = "1.2.1"
let current_file = ref ""
let debug_file = ref ""

let set_current_file f  = current_file := f
let get_current_file () = !current_file
let get_module_name () =
  Filename.chop_extension
    (Filename.basename (String.uncapitalize !current_file))

let iset_debug_file _ = ()
let set_debug_file  f = debug_file := f
let get_debug_file  () = !debug_file


let is_same_file () =
  !debug_file <> "" &&  !debug_file = !current_file

let tool_name = "BuckleScript"

let check_div_by_zero = ref true
let get_check_div_by_zero () = !check_div_by_zero

let no_any_assert = ref false

let set_no_any_assert () = no_any_assert := true
let get_no_any_assert () = !no_any_assert

let better_errors = ref false
let sort_imports = ref false
let dump_js = ref false

let is_windows =
  match Sys.os_type with
  | "Win32"
  | "Cygwin"-> true
  | _ -> false

let syntax_only = ref false
let binary_ast = ref false

end
module String_map : sig 
#1 "string_map.mli"
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








include Map.S with type key = string 

val of_list : (key * 'a) list -> 'a t

end = struct
#1 "string_map.ml"
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








include Map.Make(String)

let of_list (xs : ('a * 'b) list ) = 
  List.fold_left (fun acc (k,v) -> add k v acc) empty xs 

end
module Ast_extract : sig 
#1 "ast_extract.mli"
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




type _ kind =
  | Ml_kind : Parsetree.structure kind
  | Mli_kind : Parsetree.signature kind






module String_set = Depend.StringSet

val read_parse_and_extract : 'a kind -> 'a -> String_set.t

type ('a,'b) t 

val sort_files_by_dependencies :
  domain:String_set.t -> String_set.t String_map.t -> string Queue.t


val sort :
  ('a -> Parsetree.structure) ->
  ('b -> Parsetree.signature) ->
  ('a, 'b) t String_map.t -> string Queue.t  



(**
   [build fmt files parse_implementation parse_interface]
   Given a list of files return an ast table 
*)
val collect_ast_map :
  Format.formatter ->
  string list ->
  (Format.formatter -> string -> 'a) ->
  (Format.formatter -> string -> 'b) ->
  ('a, 'b) t String_map.t


val collect_from_main :
  ?extra_dirs:[`Dir of string  | `Dir_with_excludes of string * string list] list -> 
  ?excludes : string list -> 
  Format.formatter ->
  (Format.formatter -> string -> 'a) ->
  (Format.formatter -> string -> 'b) ->
  ('a -> Parsetree.structure) ->
  ('b -> Parsetree.signature) ->
  string -> ('a, 'b) t String_map.t * string Queue.t

val build_queue :
  Format.formatter ->
  string Queue.t ->
  ('b, 'c) t String_map.t ->
  (Format.formatter -> string -> string -> 'b -> unit) ->
  (Format.formatter -> string -> string -> 'c -> unit) -> unit
  
val handle_queue :
  Format.formatter ->
  String_map.key Queue.t ->
  ('a, 'b) t String_map.t ->
  (string -> string -> 'a -> unit) ->
  (string -> string -> 'b  -> unit) ->
  (string -> string -> string -> 'b -> 'a -> unit) -> unit


val build_lazy_queue :
  Format.formatter ->
  string Queue.t ->
  (Parsetree.structure lazy_t, Parsetree.signature lazy_t) t String_map.t ->
  (Format.formatter -> string -> string -> Parsetree.structure -> unit) ->
  (Format.formatter -> string -> string -> Parsetree.signature -> unit) -> unit  

end = struct
#1 "ast_extract.ml"
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

type module_name = private string

module String_set = Depend.StringSet

type _ kind =
  | Ml_kind : Parsetree.structure kind
  | Mli_kind : Parsetree.signature kind
        
let read_parse_and_extract (type t) (k : t kind) (ast : t) : String_set.t =
  Depend.free_structure_names := String_set.empty;
  let bound_vars = String_set.empty in
  List.iter
    (fun modname  ->
       Depend.open_module bound_vars (Longident.Lident modname))
    (!Clflags.open_modules);
  (match k with
   | Ml_kind  -> Depend.add_implementation bound_vars ast
   | Mli_kind  -> Depend.add_signature bound_vars ast  ); 
  !Depend.free_structure_names


type ('a,'b) ast_info =
  | Ml of
      string * (* sourcefile *)
      'a *
      string (* opref *)      
  | Mli of string * (* sourcefile *)
           'b *
           string (* opref *)
  | Ml_mli of
      string * (* sourcefile *)
      'a *
      string  * (* opref1 *)
      string * (* sourcefile *)      
      'b *
      string (* opref2*)

type ('a,'b) t =
  { module_name : string ; ast_info : ('a,'b) ast_info }


(* only visit nodes that are currently in the domain *)
(* https://en.wikipedia.org/wiki/Topological_sorting *)
(* dfs   *)
let sort_files_by_dependencies ~domain dependency_graph =
  let next current =
    (String_map.find  current dependency_graph) in    
  let worklist = ref domain in
  let result = Queue.create () in
  let rec visit visiting path current =
    if String_set.mem current visiting then
      Bs_exception.error (Bs_cyclic_depends (current::path))
    else if String_set.mem current !worklist then
      begin
        next current |>        
        String_set.iter
          (fun node ->
             if  String_map.mem node  dependency_graph then
               visit (String_set.add current visiting) (current::path) node)
        ;
        worklist := String_set.remove  current !worklist;
        Queue.push current result ;
      end in        
  while not (String_set.is_empty !worklist) do 
    visit String_set.empty []  (String_set.choose !worklist)
  done;
  if Js_config.get_diagnose () then
    Format.fprintf Format.err_formatter
      "Order: @[%a@]@."    
      (Ext_format.pp_print_queue
         ~pp_sep:Format.pp_print_space
         Format.pp_print_string)
      result ;       
  result
;;



let sort  project_ml project_mli (ast_table : _ t String_map.t) = 
  let domain =
    String_map.fold
      (fun k _ acc -> String_set.add k acc)
      ast_table String_set.empty in
  let h =
    String_map.map
      (fun
        ({ast_info})
        ->
          match ast_info with
          | Ml (_, ast,  _)
            ->
            read_parse_and_extract Ml_kind (project_ml ast)            
          | Mli (_, ast, _)
            ->
            read_parse_and_extract Mli_kind (project_mli ast)
          | Ml_mli (_, impl, _, _, intf, _)
            ->
            String_set.union
              (read_parse_and_extract Ml_kind (project_ml impl))
              (read_parse_and_extract Mli_kind (project_mli intf))              
      ) ast_table in    
  sort_files_by_dependencies  domain h

(** same as {!Ocaml_parse.check_suffix} but does not care with [-c -o] option*)
let check_suffix  name  = 
  if Filename.check_suffix name ".ml"
  || Filename.check_suffix name ".mlt" then 
    `Ml,
    Ext_filename.chop_extension_if_any  name 
  else if Filename.check_suffix name !Config.interface_suffix then 
    `Mli,   Ext_filename.chop_extension_if_any  name 
  else 
    raise(Arg.Bad("don't know what to do with " ^ name))


let collect_ast_map ppf files parse_implementation parse_interface  =
  List.fold_left
    (fun (acc : _ t String_map.t)
      source_file ->
      match check_suffix source_file with
      | `Ml, opref ->
        let module_name = Ext_filename.module_name_of_file source_file in
        begin match String_map.find module_name acc with
          | exception Not_found ->
            String_map.add module_name
              {ast_info =
                 (Ml (source_file, parse_implementation
                        ppf source_file, opref));
               module_name ;
              } acc
          | {ast_info = (Ml (source_file2, _, _)
                        | Ml_mli(source_file2, _, _,_,_,_))} ->
            Bs_exception.error
              (Bs_duplicated_module (source_file, source_file2))
          | {ast_info =  Mli (source_file2, intf, opref2)}
            ->
            String_map.add module_name
              {ast_info =
                 Ml_mli (source_file,
                         parse_implementation ppf source_file,
                         opref,
                         source_file2,
                         intf,
                         opref2
                        );
               module_name} acc
        end
      | `Mli, opref ->
        let module_name = Ext_filename.module_name_of_file source_file in
        begin match String_map.find module_name acc with
          | exception Not_found ->
            String_map.add module_name
              {ast_info = (Mli (source_file, parse_interface
                                              ppf source_file, opref));
               module_name } acc
          | {ast_info =
               (Mli (source_file2, _, _) |
                Ml_mli(_,_,_,source_file2,_,_)) } ->
            Bs_exception.error
              (Bs_duplicated_module (source_file, source_file2))
          | {ast_info = Ml (source_file2, impl, opref2)}
            ->
            String_map.add module_name
              {ast_info =
                 Ml_mli
                   (source_file2,
                    impl,
                    opref2,
                    source_file,
                    parse_interface ppf source_file,
                    opref
                   );
               module_name} acc
        end
    ) String_map.empty files


let collect_from_main 
    ?(extra_dirs=[])
    ?(excludes=[])
    (ppf : Format.formatter)
    parse_implementation
    parse_interface
    project_impl 
    project_intf 
    main_file =
  let not_excluded  = 
    match excludes with 
    | [] -> fun _ -> true
    | _ -> 
      fun source_file -> not (List.mem source_file excludes)
  in 
  let dirname = Filename.dirname main_file in
  (** TODO: same filename module detection  *)
  let files = 
    Array.fold_left (fun acc source_file ->
        if (Ext_string.ends_with source_file ".ml" ||
            Ext_string.ends_with source_file ".mli") 
           && not_excluded source_file
        then 
          (Filename.concat dirname source_file) :: acc else acc ) []   
      (Sys.readdir dirname) in 
  let files = 
    List.fold_left (fun acc dir_spec -> 
        let  dirname, excludes = 
          match dir_spec with 
          | `Dir dirname -> dirname, excludes
          | `Dir_with_excludes (dirname, dir_excludes) ->
            dirname,
            Ext_list.flat_map 
              (fun x -> [x ^ ".ml" ; x ^ ".mli" ])
              dir_excludes @ excludes
        in 
        Array.fold_left (fun acc source_file -> 
            if (Ext_string.ends_with source_file ".ml" ||
               Ext_string.ends_with source_file ".mli" )
               && (* not_excluded source_file *) (not (List.mem source_file excludes))
            then 
              (Filename.concat dirname source_file) :: acc else acc
          ) acc (Sys.readdir dirname))
      files extra_dirs in
  let ast_table = collect_ast_map ppf files parse_implementation parse_interface in 
  let visited = Hashtbl.create 31 in
  let result = Queue.create () in  
  let next module_name =
    match String_map.find module_name ast_table with
    | exception _ -> String_set.empty
    | {ast_info = Ml (_,  impl, _)} ->
      read_parse_and_extract Ml_kind (project_impl impl)
    | {ast_info = Mli (_,  intf,_)} ->
      read_parse_and_extract Mli_kind (project_intf intf)
    | {ast_info = Ml_mli(_, impl, _, _,  intf, _)}
      -> 
      String_set.union
        (read_parse_and_extract Ml_kind (project_impl impl))
        (read_parse_and_extract Mli_kind (project_intf intf))
  in
  let rec visit visiting path current =
    if String_set.mem current visiting  then
      Bs_exception.error (Bs_cyclic_depends (current::path))
    else
    if not (Hashtbl.mem visited current)
    && String_map.mem current ast_table then
      begin
        String_set.iter
          (visit
             (String_set.add current visiting)
             (current::path))
          (next current) ;
        Queue.push current result;
        Hashtbl.add visited current ();
      end in
  visit (String_set.empty) [] (Ext_filename.module_name_of_file main_file) ;
  ast_table, result   


let build_queue ppf queue
    (ast_table : _ t String_map.t)
    after_parsing_impl
    after_parsing_sig    
  =
  queue
  |> Queue.iter
    (fun modname -> 
      match String_map.find modname ast_table  with
      | {ast_info = Ml(source_file,ast, opref)}
        -> 
        after_parsing_impl ppf source_file 
          opref ast 
      | {ast_info = Mli (source_file,ast,opref) ; }  
        ->
        after_parsing_sig ppf source_file 
          opref ast 
      | {ast_info = Ml_mli(source_file1,impl,opref1,source_file2,intf,opref2)}
        -> 
        after_parsing_sig ppf source_file1 opref1 intf ;
        after_parsing_impl ppf source_file2 opref2 impl
      | exception Not_found -> assert false 
    )


let handle_queue ppf queue ast_table decorate_module_only decorate_interface_only decorate_module = 
  queue 
  |> Queue.iter
    (fun base ->
       match (String_map.find  base ast_table).ast_info with
       | exception Not_found -> assert false
       | Ml (ml_name,  ml_content, _)
         ->
         decorate_module_only  base ml_name ml_content
       | Mli (mli_name , mli_content, _) ->
         decorate_interface_only base  mli_name mli_content
       | Ml_mli (ml_name, ml_content, _, mli_name,   mli_content, _)
         ->
         decorate_module  base mli_name ml_name mli_content ml_content

    )



let build_lazy_queue ppf queue (ast_table : _ t String_map.t)
    after_parsing_impl
    after_parsing_sig    
  =
  queue |> Queue.iter (fun modname -> 
      match String_map.find modname ast_table  with
      | {ast_info = Ml(source_file,lazy ast, opref)}
        -> 
        after_parsing_impl ppf source_file opref ast 
      | {ast_info = Mli (source_file,lazy ast,opref) ; }  
        ->
        after_parsing_sig ppf source_file opref ast 
      | {ast_info = Ml_mli(source_file1,lazy impl,opref1,source_file2,lazy intf,opref2)}
        -> 
        after_parsing_sig ppf source_file1 opref1 intf ;
        after_parsing_impl ppf source_file2 opref2 impl
      | exception Not_found -> assert false 
    )

end
module Binary_ast : sig 
#1 "binary_ast.mli"
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


type _ kind = 
  | Ml : Parsetree.structure kind 
  | Mli : Parsetree.signature kind 

val read_ast : 'a kind -> string -> 'a 

(**
   The [.ml] file can be recognized as an ast directly, the format
   is
   {
   magic number;
   filename;
   ast
   }
   when [fname] is "-" it means the file is from an standard input or pipe.
   An empty name would marshallized.

   Use case cat - | fan -printer -impl -
   redirect the standard input to fan
 *)
val write_ast : fname:string -> output:string -> 'a kind -> 'a -> unit


type ml_kind =
  | Ml of string 
  | Re of string 
  | Ml_empty
type mli_kind = 
  | Mli of string 
  | Rei of string
  | Mli_empty

type module_info = 
  {
    mli : mli_kind ; 
    ml : ml_kind ; 
    mll : string option 
  }

val write_build_cache : string -> module_info String_map.t -> unit

val read_build_cache : string -> module_info String_map.t

end = struct
#1 "binary_ast.ml"

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


type _ kind = 
  | Ml : Parsetree.structure kind 
  | Mli : Parsetree.signature kind 


let read_ast (type t ) (kind : t kind) fn : t  =
  let magic =
    match kind with 
    | Ml -> Config.ast_impl_magic_number
    | Mli -> Config.ast_intf_magic_number in 
  let ic = open_in_bin fn in
  try
    let buffer = really_input_string ic (String.length magic) in
    assert(buffer = magic); (* already checked by apply_rewriter *)
    Location.input_name := input_value ic;
    let ast = input_value ic in
    close_in ic;
    ast
  with exn ->
    close_in ic;
    raise exn


let write_ast (type t) ~(fname : string) ~output (kind : t kind) ( pt : t) : unit =
  let magic = 
    match kind with 
    | Ml -> Config.ast_impl_magic_number
    | Mli -> Config.ast_intf_magic_number in
  let oc = open_out output in 
  output_string oc magic ;
  output_value oc fname;
  output_value oc pt;
  close_out oc 

type ml_kind =
  | Ml of string 
  | Re of string 
  | Ml_empty
type mli_kind = 
  | Mli of string 
  | Rei of string
  | Mli_empty

type module_info = 
  {
    mli : mli_kind ; 
    ml : ml_kind ; 
    mll : string option 
  }

let module_info_magic_number = "BSBUILD20161012"

let write_build_cache bsbuild (bs_files : module_info String_map.t)  = 
  let oc = open_out_bin bsbuild in 
  output_string oc module_info_magic_number ;
  output_value oc bs_files ;
  close_out oc 

let read_build_cache bsbuild : module_info String_map.t = 
  let ic = open_in bsbuild in 
  let buffer = really_input_string ic (String.length module_info_magic_number) in
  assert(buffer = module_info_magic_number); 
  let data : module_info String_map.t = input_value ic in 
  close_in ic ;
  data 

end
module Ext_array : sig 
#1 "ext_array.mli"
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






(** Some utilities for {!Array} operations *)

val reverse_in_place : 'a array -> unit

val reverse_of_list : 'a list -> 'a array

val filter : ('a -> bool) -> 'a array -> 'a array

val filter_map : ('a -> 'b option) -> 'a array -> 'b array

val range : int -> int -> int array

val map2i : (int -> 'a -> 'b -> 'c ) -> 'a array -> 'b array -> 'c array

val to_list_map : ('a -> 'b option) -> 'a array -> 'b list 

val rfind_with_index : 'a array -> ('a -> 'b -> bool) -> 'b -> int

val rfind_and_split : 
  'a array ->
  ('a -> 'b -> bool) ->
  'b -> [ `No_split | `Split of 'a array * 'a array ]

end = struct
#1 "ext_array.ml"
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







let reverse_in_place a =
  let aux a i len =
    if len=0 then ()
    else
      for k = 0 to (len-1)/2 do
        let t = Array.unsafe_get a (i+k) in
        Array.unsafe_set a (i+k) ( Array.unsafe_get a (i+len-1-k));
        Array.unsafe_set a (i+len-1-k) t;
      done
  in
  aux a 0 (Array.length a)


let reverse_of_list =  function
  | [] -> [||]
  | hd::tl as l ->
    let len = List.length l in
    let a = Array.make len hd in
    let rec fill i = function
      | [] -> a
      | hd::tl -> Array.unsafe_set a (len - i - 2) hd; fill (i+1) tl in
    fill 0 tl

let filter f a =
  let arr_len = Array.length a in
  let rec aux acc i =
    if i = arr_len 
    then reverse_of_list acc 
    else
      let v = Array.unsafe_get a i in
      if f  v then 
        aux (v::acc) (i+1)
      else aux acc (i + 1) 
  in aux [] 0


let filter_map (f : _ -> _ option) a =
  let arr_len = Array.length a in
  let rec aux acc i =
    if i = arr_len 
    then reverse_of_list acc 
    else
      let v = Array.unsafe_get a i in
      match f  v with 
      | Some v -> 
        aux (v::acc) (i+1)
      | None -> 
        aux acc (i + 1) 
  in aux [] 0

let range from to_ =
  if from > to_ then invalid_arg "Ext_array.range"  
  else Array.init (to_ - from + 1) (fun i -> i + from)

let map2i f a b = 
  let len = Array.length a in 
  if len <> Array.length b then 
    invalid_arg "Ext_array.map2i"  
  else
    Array.mapi (fun i a -> f i  a ( Array.unsafe_get b i )) a 

let to_list_map f a =
  let rec tolist i res =
    if i < 0 then res else
      let v = Array.unsafe_get a i in
      tolist (i - 1)
        (match f v with
         | Some v -> v :: res
         | None -> res) in
  tolist (Array.length a - 1) []

(**
{[
# rfind_with_index [|1;2;3|] (=) 2;;
- : int = 1
# rfind_with_index [|1;2;3|] (=) 1;;
- : int = 0
# rfind_with_index [|1;2;3|] (=) 3;;
- : int = 2
# rfind_with_index [|1;2;3|] (=) 4;;
- : int = -1
]}
*)
let rfind_with_index arr cmp v = 
  let len = Array.length arr in 
  let rec aux i = 
    if i < 0 then i
    else if  cmp (Array.unsafe_get arr i) v then i
    else aux (i - 1) in 
  aux (len - 1)

let rfind_and_split arr cmp v = 
  let i = rfind_with_index arr cmp v in 
  if  i < 0 then 
    `No_split 
  else 
    `Split (Array.sub arr 0 i , Array.sub arr  (i + 1 ) (Array.length arr - i - 1 ))

end
module Sexp_lexer : sig 
#1 "sexp_lexer.mli"
type t  =  
  | Atom of string 
  | List of t list
  | Data of t list 
  | Lit of string 


val token :  Lexing.lexbuf ->  t list

val from_file : string -> t list 


end = struct
#1 "sexp_lexer.ml"
# 1 "sexp_lexer.gen.mll"
 

type error =
  | Illegal_character of char
  | Illegal_escape of string
  | Unbalanced_paren 
  | Unterminated_paren
  | Non_sexp_outside
exception Error of error * Lexing.position * Lexing.position;;

let error  (lexbuf : Lexing.lexbuf) e = 
  raise (Error (e, lexbuf.lex_start_p, lexbuf.lex_curr_p))


let char_for_backslash = function
  | 'n' -> '\010'
  | 'r' -> '\013'
  | 'b' -> '\008'
  | 't' -> '\009'
  | c -> c

let lf = '\010'

let dec_code c1 c2 c3 =
  100 * (Char.code c1 - 48) + 10 * (Char.code c2 - 48) + (Char.code c3 - 48)

let hex_code c1 c2 =
  let d1 = Char.code c1 in
  let val1 =
    if d1 >= 97 then d1 - 87
    else if d1 >= 65 then d1 - 55
    else d1 - 48 in
  let d2 = Char.code c2 in
  let val2 =
    if d2 >= 97 then d2 - 87
    else if d2 >= 65 then d2 - 55
    else d2 - 48 in
  val1 * 16 + val2

let update_loc ({ lex_curr_p; _ } as lexbuf : Lexing.lexbuf) diff =
  lexbuf.lex_curr_p <-
    {
      lex_curr_p with
      pos_lnum = lex_curr_p.pos_lnum + 1;
      pos_bol = lex_curr_p.pos_cnum - diff;
    }

let lexeme_len ({ lex_start_pos; lex_curr_pos; _ } : Lexing.lexbuf) =
  lex_curr_pos - lex_start_pos



type t  =
  | Atom of string 
  | List of t list
  | Data of t list 
  | Lit of string 



type st = 
  { sexps : (t list * bool) Stack.t ; 
    mutable top : t list   ;
    mutable has_prime : bool ;
    buf : Buffer.t;
    mutable paren_depth : int
  }

let push_atom lexbuf atom (buf : st ) = 
  buf.top <- atom:: buf.top

(** entering the new stack *)
let new_lparen has_prime buf = 
  buf.paren_depth <- buf.paren_depth + 1 ;
  Stack.push (buf.top, buf.has_prime) buf.sexps ;
  buf.top <- [];
  buf.has_prime <- has_prime

(** exit the stack *)
let new_rparen  buf lexbuf = 
  buf.paren_depth <- buf.paren_depth - 1 ; 
  if buf.paren_depth < 0  then
    error lexbuf Unbalanced_paren
  else 
    let new_sexp =
      if buf.has_prime then 
        Data (List.rev   buf.top)
      else List (List.rev   buf.top) 
    in 
    let top, has_prime =  Stack.pop buf.sexps in
    buf.top<- top;
    buf.has_prime<-has_prime;
    push_atom lexbuf new_sexp buf 

let get_data buf = buf.top


# 100 "sexp_lexer.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base = 
   "\000\000\246\255\247\255\078\000\249\255\250\255\251\255\002\000\
    \001\000\006\000\006\000\255\255\252\255\191\000\246\255\192\000\
    \248\255\195\000\255\255\249\255\012\001\161\000\252\255\007\000\
    \011\000\012\000\210\000\251\255\035\001\250\255";
  Lexing.lex_backtrk = 
   "\255\255\255\255\255\255\007\000\255\255\255\255\255\255\008\000\
    \002\000\001\000\008\000\255\255\255\255\255\255\255\255\008\000\
    \255\255\255\255\255\255\255\255\006\000\006\000\255\255\006\000\
    \001\000\002\000\255\255\255\255\255\255\255\255";
  Lexing.lex_default = 
   "\002\000\000\000\000\000\255\255\000\000\000\000\000\000\255\255\
    \008\000\255\255\255\255\000\000\000\000\015\000\000\000\015\000\
    \000\000\019\000\000\000\000\000\255\255\255\255\000\000\255\255\
    \255\255\255\255\255\255\000\000\255\255\000\000";
  Lexing.lex_trans = 
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\009\000\011\000\255\255\009\000\010\000\255\255\009\000\
    \011\000\025\000\009\000\000\000\024\000\025\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \009\000\000\000\004\000\000\000\000\000\000\000\009\000\007\000\
    \006\000\005\000\012\000\024\000\025\000\003\000\003\000\000\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\000\000\008\000\000\000\003\000\000\000\003\000\
    \000\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\000\000\000\000\000\000\000\000\003\000\
    \000\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\000\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \000\000\000\000\000\000\003\000\000\000\003\000\000\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\000\000\000\000\000\000\000\000\003\000\000\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\016\000\255\255\000\000\000\000\024\000\000\000\000\000\
    \023\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\026\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\018\000\255\255\022\000\000\000\022\000\000\000\000\000\
    \000\000\000\000\022\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\000\000\000\000\000\000\
    \001\000\255\255\027\000\027\000\027\000\027\000\027\000\027\000\
    \027\000\027\000\027\000\027\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\017\000\255\255\000\000\000\000\022\000\
    \000\000\000\000\000\000\000\000\000\000\022\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\022\000\000\000\000\000\000\000\022\000\000\000\022\000\
    \000\000\000\000\000\000\020\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\000\000\000\000\000\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\014\000\
    \255\255\000\000\000\000\255\255\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000";
  Lexing.lex_check = 
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\008\000\000\000\000\000\008\000\009\000\
    \010\000\023\000\009\000\255\255\024\000\025\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\000\000\255\255\255\255\255\255\009\000\000\000\
    \000\000\000\000\007\000\024\000\025\000\000\000\000\000\255\255\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\255\255\000\000\255\255\000\000\255\255\000\000\
    \255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\255\255\255\255\255\255\255\255\000\000\
    \255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\003\000\003\000\255\255\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \255\255\255\255\255\255\003\000\255\255\003\000\255\255\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\255\255\255\255\255\255\255\255\003\000\255\255\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\013\000\015\000\255\255\255\255\017\000\255\255\255\255\
    \017\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\013\000\015\000\017\000\255\255\017\000\255\255\255\255\
    \255\255\255\255\017\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\255\255\255\255\255\255\
    \000\000\008\000\026\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\026\000\026\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\013\000\015\000\255\255\255\255\017\000\
    \255\255\255\255\255\255\255\255\255\255\017\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\017\000\255\255\255\255\255\255\017\000\255\255\017\000\
    \255\255\255\255\255\255\017\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\020\000\020\000\020\000\
    \020\000\020\000\020\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\028\000\028\000\028\000\028\000\
    \028\000\028\000\255\255\255\255\255\255\020\000\020\000\020\000\
    \020\000\020\000\020\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\028\000\028\000\028\000\028\000\
    \028\000\028\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\013\000\
    \015\000\255\255\255\255\017\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255";
  Lexing.lex_base_code = 
   "";
  Lexing.lex_backtrk_code = 
   "";
  Lexing.lex_default_code = 
   "";
  Lexing.lex_trans_code = 
   "";
  Lexing.lex_check_code = 
   "";
  Lexing.lex_code = 
   "";
}

let rec main buf lexbuf =
    __ocaml_lex_main_rec buf lexbuf 0
and __ocaml_lex_main_rec buf lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 110 "sexp_lexer.gen.mll"
                     ( 
    update_loc lexbuf 0;
    main (buf : st ) lexbuf  )
# 280 "sexp_lexer.ml"

  | 1 ->
# 113 "sexp_lexer.gen.mll"
           ( main buf lexbuf  )
# 285 "sexp_lexer.ml"

  | 2 ->
# 114 "sexp_lexer.gen.mll"
                       (  main buf lexbuf )
# 290 "sexp_lexer.ml"

  | 3 ->
# 115 "sexp_lexer.gen.mll"
         (
    new_lparen true buf; 
    main buf lexbuf
  )
# 298 "sexp_lexer.ml"

  | 4 ->
# 119 "sexp_lexer.gen.mll"
        ( 
    new_lparen false buf ; 
    main buf lexbuf 
  )
# 306 "sexp_lexer.ml"

  | 5 ->
# 123 "sexp_lexer.gen.mll"
        ( 
      new_rparen  buf lexbuf; 
      main buf lexbuf 
  )
# 314 "sexp_lexer.ml"

  | 6 ->
# 128 "sexp_lexer.gen.mll"
      (
        let pos = Lexing.lexeme_start_p lexbuf in
        scan_string buf.buf pos lexbuf;
        push_atom lexbuf  ( Lit (Buffer.contents  buf.buf)) buf;
        Buffer.clear buf.buf;
        main buf lexbuf
      )
# 325 "sexp_lexer.ml"

  | 7 ->
let
# 135 "sexp_lexer.gen.mll"
                    s
# 331 "sexp_lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 136 "sexp_lexer.gen.mll"
    ( push_atom lexbuf (Atom s) buf ; 
      main buf lexbuf
    )
# 337 "sexp_lexer.ml"

  | 8 ->
let
# 139 "sexp_lexer.gen.mll"
         c
# 343 "sexp_lexer.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 140 "sexp_lexer.gen.mll"
      (  error  lexbuf (Illegal_character c))
# 347 "sexp_lexer.ml"

  | 9 ->
# 142 "sexp_lexer.gen.mll"
        (
    if buf.paren_depth > 0 then 
      error lexbuf Unterminated_paren
    else 
      get_data buf )
# 356 "sexp_lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; 
      __ocaml_lex_main_rec buf lexbuf __ocaml_lex_state

and scan_string buf start lexbuf =
    __ocaml_lex_scan_string_rec buf start lexbuf 13
and __ocaml_lex_scan_string_rec buf start lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 149 "sexp_lexer.gen.mll"
        ( () )
# 368 "sexp_lexer.ml"

  | 1 ->
# 151 "sexp_lexer.gen.mll"
      (
        let len = lexeme_len lexbuf - 2 in
        update_loc lexbuf len;

        scan_string buf start lexbuf
      )
# 378 "sexp_lexer.ml"

  | 2 ->
# 158 "sexp_lexer.gen.mll"
      (
        let len = lexeme_len lexbuf - 3 in
        update_loc lexbuf len;
        scan_string buf start lexbuf
      )
# 387 "sexp_lexer.ml"

  | 3 ->
let
# 163 "sexp_lexer.gen.mll"
                                                 c
# 393 "sexp_lexer.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 1) in
# 164 "sexp_lexer.gen.mll"
      (
        Buffer.add_char buf (char_for_backslash c);
        scan_string buf start lexbuf
      )
# 400 "sexp_lexer.ml"

  | 4 ->
let
# 168 "sexp_lexer.gen.mll"
                   c1
# 406 "sexp_lexer.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 1)
and
# 168 "sexp_lexer.gen.mll"
                                 c2
# 411 "sexp_lexer.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 2)
and
# 168 "sexp_lexer.gen.mll"
                                               c3
# 416 "sexp_lexer.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 3)
and
# 168 "sexp_lexer.gen.mll"
                                                      s
# 421 "sexp_lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos (lexbuf.Lexing.lex_start_pos + 4) in
# 169 "sexp_lexer.gen.mll"
      (
        let v = dec_code c1 c2 c3 in
        if v > 255 then
          error lexbuf (Illegal_escape s) ;
        Buffer.add_char buf (Char.chr v);

        scan_string buf start lexbuf
      )
# 432 "sexp_lexer.ml"

  | 5 ->
let
# 177 "sexp_lexer.gen.mll"
                          c1
# 438 "sexp_lexer.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 2)
and
# 177 "sexp_lexer.gen.mll"
                                           c2
# 443 "sexp_lexer.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 3) in
# 178 "sexp_lexer.gen.mll"
      (
        let v = hex_code c1 c2 in
        Buffer.add_char buf (Char.chr v);

        scan_string buf start lexbuf
      )
# 452 "sexp_lexer.ml"

  | 6 ->
let
# 184 "sexp_lexer.gen.mll"
               c
# 458 "sexp_lexer.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 1) in
# 185 "sexp_lexer.gen.mll"
      (
        Buffer.add_char buf '\\';
        Buffer.add_char buf c;

        scan_string buf start lexbuf
      )
# 467 "sexp_lexer.ml"

  | 7 ->
# 192 "sexp_lexer.gen.mll"
      (
        update_loc lexbuf 0;
        Buffer.add_char buf lf;

        scan_string buf start lexbuf
      )
# 477 "sexp_lexer.ml"

  | 8 ->
# 199 "sexp_lexer.gen.mll"
      (
        let ofs = lexbuf.lex_start_pos in
        let len = lexbuf.lex_curr_pos - ofs in
        Buffer.add_substring buf lexbuf.lex_buffer ofs len;

        scan_string buf start lexbuf
      )
# 488 "sexp_lexer.ml"

  | 9 ->
# 207 "sexp_lexer.gen.mll"
      (
        let msg =
          Printf.sprintf
            "Sexplib.Lexer.scan_string: unterminated string at line %d char %d"
            start.pos_lnum (start.pos_cnum - start.pos_bol)
        in
        failwith msg
      )
# 500 "sexp_lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; 
      __ocaml_lex_scan_string_rec buf start lexbuf __ocaml_lex_state

;;

# 216 "sexp_lexer.gen.mll"
  

    let token  lexbuf  =
      List.rev @@ main { 
        buf = Buffer.create 256 ;
        sexps = Stack.create () ; 
        paren_depth = 0; 
        top = [];
        has_prime = false } lexbuf
    let from_file file = 
      let in_channel =  open_in file in 
      match  token (Lexing.from_channel in_channel) with 
      | exception  e -> close_in in_channel; raise e 
      | sexps -> close_in in_channel ; sexps

# 523 "sexp_lexer.ml"

end
module Sexp_eval
= struct
#1 "sexp_eval.ml"
type env = (string, Sexp_lexer.t) Hashtbl.t

let rec print fmt (x : Sexp_lexer.t) =
  match x with 
  | Atom v -> 
    Format.pp_print_string fmt v
  | Lit s -> 
    Format.fprintf fmt "%S" s (* TODO: string escaping*)
  | List vs -> 
    Format.fprintf fmt  "@[(@ %a@ )@]" 
      (Format.pp_print_list ~pp_sep:(Format.pp_print_space) print ) vs 
  | Data vs
    -> 
    Format.fprintf fmt "@['(@ %a@ )@]" 
      (Format.pp_print_list ~pp_sep:(Format.pp_print_space) print) vs 

let print_env fmt  env = 
  Hashtbl.iter (fun k v -> 
      Format.fprintf fmt
        "@[%s@ ->@ %a@]@."
        k print v 
    ) env 


let nil : Sexp_lexer.t = List []  

exception Unbound_value of string 

let rec eval env (x : Sexp_lexer.t) : Sexp_lexer.t =
  match x with 
  | Atom x ->
    begin match Hashtbl.find env x  with
    | exception Not_found 
      -> raise (Unbound_value x)
    | x -> x 
    end
  | Lit _ -> x 
  | Data xs -> List xs
  | List [] -> nil
  | List (command::rest) -> 
    begin match command with 
    | Atom "setq" 
      -> let rec loop last_value rest =
           match rest with 
           | Sexp_lexer.Atom x :: v :: rest -> 
             let xvalue = eval env v in 
             Hashtbl.add env x xvalue ; 
             loop xvalue rest 
           | [Atom x] 
             ->  Hashtbl.add env x nil ; nil 
           | [] -> last_value
           | (Lit _ | Data _ | List _ ) :: _  
             -> assert false  in 
      loop nil rest
    | _ -> assert false
    end

let eval_file s = 
  let sexps = Sexp_lexer.from_file s in 
  let env = Hashtbl.create 64 in 
  List.iter (fun x -> ignore (eval env x )) sexps ; 
  env 

let eval_string s = 
  let sexps = Sexp_lexer.token (Lexing.from_string s) in 
  let env = Hashtbl.create 64 in 
  List.iter (fun x -> ignore (eval env x )) sexps ; 
  env 

end
module Bsbuild_main
= struct
#1 "bsbuild_main.ml"
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



type module_info = Binary_ast.module_info 

let bsbuild = ".bsbuild"
let main_ninja = "build.ninja"
module Flags = struct 
  let bsc = ("bsc", "bsc.exe")
  let bsbuild = ("bsbuild", "bsbuild.exe")
  let package_name = "package-name"
  let bs_package_name = "-bs-package-name"
  let ocamllex = ("ocamllex", "ocamllex.opt")
  let bs_external_includes = "bs-external-includes"
  let bsc_flags = "bsc-flags"
  let files = "files"
  let ppx_flags = "ppx-flags"
end
module String_set = Depend.StringSet 


let dep_lit = " : "
let space = " "
let handle_depfile (fn : string) : unit = 
  let data = Binary_ast.read_build_cache bsbuild in 
  let deps = 
    match Ext_string.ends_with_then_chop fn Literals.suffix_mlast with 
    | Some  file_name_sans_extension -> 
      let stru  = Binary_ast.read_ast Ml  fn in 
      let s = Ast_extract.read_parse_and_extract Ml_kind stru in 
      String_set.fold
        (fun k acc -> 
           match String_map.find k data with
           | {ml = Ml s | Re s  } 
           | {mll = Some s } 
             -> 
             acc ^ space ^ (Filename.chop_extension s) ^ Literals.suffix_cmj
           | {mli = Mli s | Rei s } -> 
             acc ^ space ^  (Filename.chop_extension s) ^ Literals.suffix_cmi
           | _ -> assert false
           | exception Not_found -> acc 
        ) s (file_name_sans_extension ^ Literals.suffix_cmj ^ dep_lit )   
    | None -> 
      begin match Ext_string.ends_with_then_chop fn Literals.suffix_mliast with 
      | Some file_name_sans_extension -> 
        let stri = Binary_ast.read_ast Mli  fn in 
        let s = Ast_extract.read_parse_and_extract Mli_kind stri in 
        String_set.fold
          (fun k acc ->
             match String_map.find k data with 
             | { ml = Ml f | Re f  }
             | { mll = Some f }
             | { mli = Mli f | Rei f } -> 
               acc ^ space ^ (Filename.chop_extension f) ^ Literals.suffix_cmi
             | _ -> assert false
             | exception Not_found -> acc 
          ) s  (file_name_sans_extension ^ Literals.suffix_cmi ^ dep_lit ) 
      | None -> 
        raise (Arg.Bad ("don't know what to do with  " ^ fn))
      end
  in 
  Ext_pervasives.with_file_as_chan (fn ^ Literals.suffix_d) (fun v -> output_string v deps)










(* TODO check duplication *)
let module_info_of_ml exist ml : module_info =
  match exist with 
  | None -> { ml  = Ml ml ; mli = Mli_empty ; mll = None }
  | Some x -> { x with ml = Ml ml}

let module_info_of_re exist ml : module_info =
  match exist with 
  | None -> { ml  = Re ml ; mli = Mli_empty ; mll = None }
  | Some x -> { x with ml = Re ml} 

let module_info_of_mli exist mli : module_info = 
  match exist with 
  | None -> { mli  = Mli mli ; ml = Ml_empty ; mll = None }
  | Some x -> { x with mli = Mli mli} 

let module_info_of_rei exist mli : module_info = 
  match exist with 
  | None -> { mli  = Rei mli ; ml = Ml_empty ; mll = None }
  | Some x -> { x with mli = Rei mli} 

let module_info_of_mll exist mll : module_info = 
  match exist with 
  | None -> { mll  = Some mll ; ml = Ml_empty ; mli = Mli_empty }
  | Some x -> { x with mll = Some mll} 


let update map name = 
  let module_name = Ext_filename.module_name_of_file name in 
  let aux v name = 
    if Filename.check_suffix name ".ml" then module_info_of_ml v name  else
    if Filename.check_suffix name ".mll" then module_info_of_mll v name else 
    if Filename.check_suffix name ".mli" then module_info_of_mli v name else 
    if Filename.check_suffix name ".re" then module_info_of_re v name else 
    if Filename.check_suffix name ".rei" then module_info_of_rei v name else 
      assert false   in 
  match String_map.find module_name map with 
  | exception Not_found 
    -> String_map.add module_name (aux None name) map 
  | v -> 
    String_map.add module_name (aux (Some v) name)  map

type ty = 
  | String 
  | List_string 

exception Expect of string * ty

let error (x,ty) = raise (Expect (x,ty) )

let expect_string (key, default) (global_data : Sexp_eval.env) =
  match Hashtbl.find global_data key with 
  | exception Not_found -> default
  | Atom s | Lit s -> s 
  | List _ | Data _ -> error (key, String)

let expect_string_opt key (global_data : Sexp_eval.env) =
  match Hashtbl.find global_data key with 
  | exception Not_found -> None
  | Atom s | Lit s -> Some s 
  | List _ | Data _ -> error (key, String)

let expect_string_list key (global_data : Sexp_eval.env) = 
  match Hashtbl.find global_data key  with 
  | exception Not_found -> [ ]
  | Atom _ | Lit _ | Data _ -> error(key, List_string)
  | List xs -> 
    Ext_list.filter_map (fun x -> 
        match  x with 
        | Sexp_lexer.Atom x | Lit x -> Some x 
        | _ -> None 
      ) xs 

let expect_string_list_unordered 
  key 
  (global_data : Sexp_eval.env) 
  init update = 
  match Hashtbl.find global_data Flags.files with 
  | exception Not_found -> init
  | Atom _ | Lit _ 
  | Data _ -> error(key, List_string)
  | List files -> 
    List.fold_left (fun acc s ->
        match s with 
        | Sexp_lexer.Atom s | Lit s -> update acc s 
        | _ -> acc (* raise Type error *)
      ) init files 

let output_ninja (global_data : Sexp_eval.env)  = 
  let bsc = expect_string Flags.bsc global_data in 
  let bsbuild = expect_string Flags.bsbuild global_data in 
  let package_name = expect_string_opt Flags.package_name global_data in 
  let ocamllex = expect_string Flags.ocamllex global_data in 
  let bs_external_includes =
    expect_string_list Flags.bs_external_includes  global_data in 
  let ppx_flags = 
    String.concat space @@
      Ext_list.flat_map (fun x -> ["-ppx";  x ])  @@
    expect_string_list Flags.ppx_flags global_data in 
  let bsc_computed_flags =
    let external_includes = 
      Ext_list.flat_map (fun x -> ["-I" ; x]) bs_external_includes in 
    let init_flags = 
      match package_name with 
      | None -> external_includes
      | Some x -> Flags.bs_package_name ::  x :: external_includes
    in 
    let bsc_flags = expect_string_list Flags.bsc_flags global_data in 
    String.concat " " ( bsc_flags @ init_flags)
  in

  let bs_files = expect_string_list_unordered Flags.files global_data String_map.empty update 
  in  
  Binary_ast.write_build_cache bsbuild bs_files ;
  let oc = open_out main_ninja in 
  begin 
    let _all_depes = ref [] in 
    let () = 
      output_string oc "bsc = "; output_string oc bsc ; output_string oc "\n";
      output_string oc "bsc_computed_flags = "; output_string oc bsc_computed_flags ; output_string oc "\n";
      output_string oc "bsbuild = "; output_string oc bsbuild ; output_string oc "\n";
      output_string oc "ocamllex = "; output_string oc ocamllex ; output_string oc "\n";
      output_string oc "ppx_flags = "; output_string oc ppx_flags ; output_string oc "\n";
      output_string oc {|
rule build_ast
  command = ${bsc} ${pp_flags} ${ppx_flags} ${bsc_computed_flags} -bs-syntax-only -bs-binary-ast ${in}
  description = Building ${out}
rule build_ast_from_reason_impl
  command = ${bsc} -pp refmt ${ppx_flags} ${bsc_computed_flags} -bs-syntax-only -bs-binary-ast -impl ${in}        
  description = Building ${out}
rule build_ast_from_reason_intf
  command = ${bsc} -pp refmt ${ppx_flags} ${bsc_computed_flags} -bs-syntax-only -bs-binary-ast -intf ${in}        
  description = Building ${out}
rule build_deps
  command = ${bsbuild} -bs-depfile ${in}
  description = Building ${out}
rule build_ml_from_mll
  command = ${ocamllex} ${in}
rule build_cmj
  depfile = ${in}.d
  command = ${bsc} -bs-no-builtin-ppx-ml ${bsc_computed_flags} -c -impl ${in}
  description = Building ${out}
rule build_cmj_without_mli
  depfile = ${in}.d
  command = ${bsc} -intf-suffix .foobar -bs-no-builtin-ppx-ml ${bsc_computed_flags} -c -impl ${in}
  description = Building ${out}
rule build_cmi
  depfile = ${in}.d
  command = ${bsc} -bs-no-builtin-ppx-mli ${bsc_computed_flags} -c -intf ${in}
  description = Building ${out}
|};
    in
    bs_files
    |> String_map.iter (fun module_name ({mli; ml; mll } : module_info) -> 
        let spit_out_ml (kind : [`Ml | `Re ])  file filename_sans_extension = 
          if kind = `Ml then 
            output_string oc (Printf.sprintf "build %s.mlast : build_ast %s\n" filename_sans_extension file)
          else 
            output_string oc (Printf.sprintf "build %s.mlast : build_ast_from_reason_impl %s\n"
                                filename_sans_extension file)
          ;
          output_string oc (Printf.sprintf "build %s.mlast.d : build_deps %s.mlast\n" filename_sans_extension
                              filename_sans_extension);
          _all_depes := (filename_sans_extension ^ Literals.suffix_mlastd) :: !_all_depes;
          let rule_name , output, deps = 
            let cmi_file = filename_sans_extension ^ Literals.suffix_cmi in
            if mli = Mli_empty then "build_cmj",  cmi_file  , ""
            else "build_cmj_without_mli", "", cmi_file  in  
          output_string oc 
            (Printf.sprintf "build %s.cmj %s : %s %s.mlast |  %s\n"
               filename_sans_extension output
               rule_name 
               filename_sans_extension deps) in 

        begin match ml with 
          | Ml ml_file -> 
            let filename_sans_extension = Filename.chop_extension ml_file in 
            spit_out_ml `Ml ml_file filename_sans_extension
          | Re re_file -> 
            let filename_sans_extension = Filename.chop_extension re_file in 
            spit_out_ml `Re re_file filename_sans_extension

          | Ml_empty -> () end;
        let spit_out_mli (kind : [`Mli | `Rei ])  mli_file filename = 
          if kind = `Mli then 
            output_string oc (Printf.sprintf "build %s.mliast : build_ast %s\n" filename mli_file)
          else
            output_string oc (Printf.sprintf "build %s.mliast : build_ast_from_reason_intf %s\n" filename mli_file);
          output_string oc (Printf.sprintf "build %s.mliast.d : build_deps %s.mliast\n" filename filename);
          output_string oc (Printf.sprintf "build %s.cmi : build_cmi %s.mliast | %s.mliast.d\n"
                                  filename filename filename
                               );
          _all_depes := (filename ^ Literals.suffix_mliastd) :: !_all_depes in 

        begin match mli with 
          | Mli mli_file  -> 
            let filename = Filename.chop_extension mli_file in 
            spit_out_mli `Mli mli_file filename
          | Rei rei_file -> 
            let filename = Filename.chop_extension rei_file in 
            spit_out_mli `Rei rei_file filename
          | Mli_empty -> ()
        end;
        begin match mll with 
          | Some mll_file -> 
            (* if ml already exists then no need generate same *)
              let filename = Filename.chop_extension mll_file in
              if ml = Ml_empty then 
                spit_out_ml `Ml (filename ^ Literals.suffix_ml) filename;
              output_string oc (Printf.sprintf "build %s.ml : build_ml_from_mll %s.mll\n" filename filename);
          | None -> ()
        end
      );
    output_string oc  {|
rule reload
      command = ${bsbuild} -init
build build.ninja : reload | bs.el .bsbuild
|};
    output_string oc (Printf.sprintf "build config : phony %s\n" (String.concat " "  !_all_depes)) ;
    close_out oc;
  end

let write_ninja_file () = 
  let global_data = Sexp_eval.eval_file "bs.el" in 
  output_ninja global_data 

let load_ninja = ref false

let load_ninja ninja_flags = 
  let ninja = "ninja" in
  Unix.execvp ninja
    (Array.concat 
       [
         [|ninja ; "-d"; "keepdepfile"|];
         ninja_flags
       ]
    )
let bsninja_flags =
  [
    "-bs-depfile", Arg.String handle_depfile ,
    " generate deps file for ml";
    "-init", Arg.Unit write_ninja_file,
    " generate build.ninja";
    (* "--", Arg.Rest collect_ninja_flags, *)
    (* " flags passed to ninja" *)
 ]

let usage = {|Usage: bsbuild.exe <options> <files>
Options are:|}

let anonymous arg =
  raise (Arg.Bad ("don't know what to do with " ^ arg))

let () = 
  try 
    begin match Ext_array.rfind_and_split Sys.argv Ext_string.equal "-" with 
      | `No_split ->       Arg.parse_argv Sys.argv  bsninja_flags anonymous usage
      | `Split (bsninja_argv, ninja_flags) 
        -> 
        Arg.parse_argv bsninja_argv bsninja_flags anonymous usage;
        load_ninja ninja_flags
    end
  with x ->
    begin
      Location.report_exception Format.err_formatter x ;
      exit 2
    end






end
