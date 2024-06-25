(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Jerome Vouillon, projet Cristal, INRIA Rocquencourt          *)
(*                                                                        *)
(*   Copyright 1997 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Inclusion checks for the class language *)


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
  | CM_Parameter_arity_mismatch _ ->
      fprintf ppf
        "The classes do not have the same number of type parameters"
  | CM_Type_parameter_mismatch (env, trace) ->
      Printtyp.report_unification_error ppf env ~unif:false trace
        (function ppf ->
          fprintf ppf "A type parameter has type")
        (function ppf ->
          fprintf ppf "but is expected to have type")
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
      fprintf ppf "@[The first class type has no field %s@]" lab
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
