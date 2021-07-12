(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Jerome Vouillon, projet Cristal, INRIA Rocquencourt          *)
(*           OCaml port by John Malecki and Xavier Leroy                  *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Env

type error =
    Module_not_found of Path.t

exception Error of error

let env_cache =
  (Hashtbl.create 59 : ((Env.summary * Subst.t), Env.t) Hashtbl.t)

let reset_cache () =
  Hashtbl.clear env_cache;
  Env.reset_cache()

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
          Env.add_module_declaration ~check:false id
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
          begin match Env.open_signature Asttypes.Override path' env with
          | Some env -> env
          | None -> assert false
          end
      | Env_functor_arg(Env_module(s, id, desc), id') when Ident.same id id' ->
          Env.add_module_declaration ~check:false
            id (Subst.module_declaration subst desc)
            ~arg:true (env_from_summary s subst)
      | Env_functor_arg _ -> assert false
      | Env_constraints(s, map) ->
          PathMap.fold
            (fun path info ->
              Env.add_local_type (Subst.type_path subst path)
                (Subst.type_declaration subst info))
            map (env_from_summary s subst)
      | Env_copy_types (s, sl) ->
          Env.copy_types sl (env_from_summary s subst)
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
