(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*      Fabrice Le Fessant, EPI Gallium, INRIA Paris-Rocquencourt      *)
(*                                                                     *)
(*  Copyright 2013 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open Compenv

(* Initialize the search path.
   The current directory is always searched first,
   then the directories specified with the -I option (in command-line order),
   then the standard library directory (unless the -nostdlib option is given).
 *)

let init_path native =
  let dirs =
    if !Clflags.use_threads then "+threads" :: !Clflags.include_dirs
    else if !Clflags.use_vmthreads && not native then
      "+vmthreads" :: !Clflags.include_dirs
    else
      !last_include_dirs @
      !Clflags.include_dirs @
      !first_include_dirs
  in
  let exp_dirs =
    List.map (Misc.expand_directory Config.standard_library) dirs in
#if undefined BS_NO_COMPILER_PATCH then 
    Config.load_path :=
      (if !Clflags.no_implicit_current_dir then 
         List.rev_append exp_dirs (Clflags.std_include_dir ())
       else 
         "" :: List.rev_append exp_dirs (Clflags.std_include_dir ()));
#else
  Config.load_path := "" ::
      List.rev_append exp_dirs (Clflags.std_include_dir ());
#end
  Env.reset_cache ()

(* Return the initial environment in which compilation proceeds. *)

(* Note: do not do init_path() in initial_env, this breaks
   toplevel initialization (PR#1775) *)

let open_implicit_module m env =
  let open Asttypes in
  let lid = {loc = Location.in_file "command line";
             txt = Longident.Lident m } in
  snd (Typemod.type_open_ Override env lid.loc lid)

let initial_env () =
  Ident.reinit();
  let initial =
    if !Clflags.unsafe_string then Env.initial_unsafe_string
    else Env.initial_safe_string
  in
  let env =
    if !Clflags.nopervasives then initial else
    open_implicit_module "Pervasives" initial
  in
  List.fold_left (fun env m ->
    open_implicit_module m env
  ) env (!implicit_modules @ List.rev !Clflags.open_modules)
