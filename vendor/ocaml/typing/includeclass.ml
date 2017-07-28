(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*          Jerome Vouillon, projet Cristal, INRIA Rocquencourt        *)
(*                                                                     *)
(*  Copyright 1997 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Inclusion checks for the class language *)

open Types

let class_types env cty1 cty2 =
  Ctype.match_class_types env cty1 cty2

let class_type_declarations env cty1 cty2 =
  Ctype.match_class_declarations env
    cty1.clty_params cty1.clty_type
    cty2.clty_params cty2.clty_type

let class_declarations env cty1 cty2 =
  match cty1.cty_new, cty2.cty_new with
    None, Some _ ->
      [Ctype.CM_Virtual_class]
  | _ ->
      Ctype.match_class_declarations env
        cty1.cty_params cty1.cty_type
        cty2.cty_params cty2.cty_type

open Format
open Ctype

(*
let rec hide_params = function
    Tcty_arrow ("*", _, cty) -> hide_params cty
  | cty -> cty
*)

let include_err ppf =
  function
  | CM_Virtual_class ->
      fprintf ppf "A class cannot be changed from virtual to concrete"
  | CM_Parameter_arity_mismatch (ls, lp) ->
      fprintf ppf
        "The classes do not have the same number of type parameters"
  | CM_Type_parameter_mismatch (env, trace) ->
      Printtyp.report_unification_error ppf env ~unif:false trace
        (function ppf ->
          fprintf ppf "A type parameter has type")
        (function ppf ->
          fprintf ppf "but is expected to have type")
  | CM_Class_type_mismatch (env, cty1, cty2) ->
      Printtyp.wrap_printing_env env (fun () ->
        fprintf ppf
          "@[The class type@;<1 2>%a@ %s@;<1 2>%a@]"
          Printtyp.class_type cty1
          "is not matched by the class type"
          Printtyp.class_type cty2)
  | CM_Parameter_mismatch (env, trace) ->
      Printtyp.report_unification_error ppf env ~unif:false trace
        (function ppf ->
          fprintf ppf "A parameter has type")
        (function ppf ->
          fprintf ppf "but is expected to have type")
  | CM_Val_type_mismatch (lab, env, trace) ->
      Printtyp.report_unification_error ppf env ~unif:false trace
        (function ppf ->
          fprintf ppf "The instance variable %s@ has type" lab)
        (function ppf ->
          fprintf ppf "but is expected to have type")
  | CM_Meth_type_mismatch (lab, env, trace) ->
      Printtyp.report_unification_error ppf env ~unif:false trace
        (function ppf ->
          fprintf ppf "The method %s@ has type" lab)
        (function ppf ->
          fprintf ppf "but is expected to have type")
  | CM_Non_mutable_value lab ->
      fprintf ppf
       "@[The non-mutable instance variable %s cannot become mutable@]" lab
  | CM_Non_concrete_value lab ->
      fprintf ppf
       "@[The virtual instance variable %s cannot become concrete@]" lab
  | CM_Missing_value lab ->
      fprintf ppf "@[The first class type has no instance variable %s@]" lab
  | CM_Missing_method lab ->
      fprintf ppf "@[The first class type has no method %s@]" lab
  | CM_Hide_public lab ->
     fprintf ppf "@[The public method %s cannot be hidden@]" lab
  | CM_Hide_virtual (k, lab) ->
      fprintf ppf "@[The virtual %s %s cannot be hidden@]" k lab
  | CM_Public_method lab ->
      fprintf ppf "@[The public method %s cannot become private" lab
  | CM_Virtual_method lab ->
      fprintf ppf "@[The virtual method %s cannot become concrete" lab
  | CM_Private_method lab ->
      fprintf ppf "The private method %s cannot become public" lab

let report_error ppf = function
  |  [] -> ()
  | err :: errs ->
      let print_errs ppf errs =
         List.iter (fun err -> fprintf ppf "@ %a" include_err err) errs in
      fprintf ppf "@[<v>%a%a@]" include_err err print_errs errs
