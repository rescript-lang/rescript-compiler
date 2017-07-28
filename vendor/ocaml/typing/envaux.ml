(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*          Jerome Vouillon, projet Cristal, INRIA Rocquencourt        *)
(*          OCaml port by John Malecki and Xavier Leroy                *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open Misc
open Types
open Env

type error =
    Module_not_found of Path.t

exception Error of error

let env_cache =
  (Hashtbl.create 59 : ((Env.summary * Subst.t), Env.t) Hashtbl.t)

let reset_cache () =
  Hashtbl.clear env_cache;
  Env.reset_cache()

let extract_sig env mty =
  match Env.scrape_alias env mty with
    Mty_signature sg -> sg
  | _ -> fatal_error "Envaux.extract_sig"

let rec env_from_summary sum subst =
  try
    Hashtbl.find env_cache (sum, subst)
  with Not_found ->
    let env =
      match sum with
        Env_empty ->
          Env.empty
      | Env_value(s, id, desc) ->
          Env.add_value id (Subst.value_description subst desc)
                        (env_from_summary s subst)
      | Env_type(s, id, desc) ->
          Env.add_type ~check:false id
            (Subst.type_declaration subst desc)
            (env_from_summary s subst)
      | Env_extension(s, id, desc) ->
          Env.add_extension ~check:false id
            (Subst.extension_constructor subst desc)
            (env_from_summary s subst)
      | Env_module(s, id, desc) ->
          Env.add_module_declaration id
            (Subst.module_declaration subst desc)
            (env_from_summary s subst)
      | Env_modtype(s, id, desc) ->
          Env.add_modtype id (Subst.modtype_declaration subst desc)
                          (env_from_summary s subst)
      | Env_class(s, id, desc) ->
          Env.add_class id (Subst.class_declaration subst desc)
                        (env_from_summary s subst)
      | Env_cltype (s, id, desc) ->
          Env.add_cltype id (Subst.cltype_declaration subst desc)
                         (env_from_summary s subst)
      | Env_open(s, path) ->
          let env = env_from_summary s subst in
          let path' = Subst.module_path subst path in
          let md =
            try
              Env.find_module path' env
            with Not_found ->
              raise (Error (Module_not_found path'))
          in
          Env.open_signature Asttypes.Override path'
            (extract_sig env md.md_type) env
      | Env_functor_arg(Env_module(s, id, desc), id') when Ident.same id id' ->
          Env.add_module_declaration id (Subst.module_declaration subst desc)
            ~arg:true (env_from_summary s subst)
      | Env_functor_arg _ -> assert false
    in
      Hashtbl.add env_cache (sum, subst) env;
      env

let env_of_only_summary env =
  Env.env_of_only_summary env_from_summary env

(* Error report *)

open Format

let report_error ppf = function
  | Module_not_found p ->
      fprintf ppf "@[Cannot find module %a@].@." Printtyp.path p
