(***********************************************************************)
(*                                                                     *)
(*                             OCamldoc                                *)
(*                                                                     *)
(*            Maxence Guesdon, projet Cristal, INRIA Rocquencourt      *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(** Main module for bytecode.
@todo coucou le todo*)

open Config
open Clflags
open Misc
open Format
open Typedtree

module M = Odoc_messages

let print_DEBUG s = print_string s ; print_newline ()

(* we check if we must load a module given on the command line *)
let arg_list = Array.to_list Sys.argv
let (plugins, paths) =
  let rec iter (files, incs) = function
      [] | _ :: [] -> (List.rev files, List.rev incs)
    | "-g" :: file :: q when
        ((Filename.check_suffix file "cmo") ||
         (Filename.check_suffix file "cma") ||
           (Filename.check_suffix file "cmxs")) ->
      iter (file :: files, incs) q
  | "-i" :: dir :: q ->
      iter (files, dir :: incs) q
  | _ :: q ->
        iter (files, incs) q
  in
  iter ([], []) arg_list

let _ = print_DEBUG "Fin analyse des arguments pour le dynamic load"

(** Return the real name of the file to load,
   searching it in the paths if it is
   a simple name and not in the current directory. *)
let get_real_filename name =
   if Filename.basename name <> name then
     name
   else
     (
      let paths = Filename.current_dir_name :: paths @ [Odoc_config.custom_generators_path] in
      try
        let d = List.find
            (fun d -> Sys.file_exists (Filename.concat d name))
            paths
        in
        Filename.concat d name
      with
        Not_found ->
          failwith (M.file_not_found_in_paths paths name)
     )

let load_plugin file =
  let file = Dynlink.adapt_filename file in
  Dynlink.allow_unsafe_modules true;
  try
    let real_file = get_real_filename file in
    ignore(Dynlink.loadfile real_file)
  with
    Dynlink.Error e ->
      prerr_endline (Odoc_messages.load_file_error file (Dynlink.error_message e)) ;
      exit 1
  | Not_found ->
      prerr_endline (Odoc_messages.load_file_error file "Not_found");
      exit 1
  | Sys_error s
  | Failure s ->
      prerr_endline (Odoc_messages.load_file_error file s);
      exit 1
;;
List.iter load_plugin plugins;;

let () = print_DEBUG "Fin du chargement dynamique eventuel"

let () = Odoc_args.parse ()


let loaded_modules =
  List.flatten
    (List.map
       (fun f ->
         Odoc_info.verbose (Odoc_messages.loading f);
         try
           let l = Odoc_analyse.load_modules f in
           Odoc_info.verbose Odoc_messages.ok;
           l
         with Failure s ->
           prerr_endline s ;
           incr Odoc_global.errors ;
           []
       )
       !Odoc_global.load
    )

let modules = Odoc_analyse.analyse_files ~init: loaded_modules !Odoc_global.files

let _ =
  match !Odoc_global.dump with
    None -> ()
  | Some f ->
      try Odoc_analyse.dump_modules f modules
      with Failure s ->
        prerr_endline s ;
        incr Odoc_global.errors


let _ =
  match !Odoc_args.current_generator with
    None ->
      ()
  | Some gen ->
      let generator = Odoc_gen.get_minimal_generator gen in
      Odoc_info.verbose Odoc_messages.generating_doc;
      generator#generate modules;
      Odoc_info.verbose Odoc_messages.ok

let _ =
  if !Odoc_global.errors > 0 then
  (
   prerr_endline (Odoc_messages.errors_occured !Odoc_global.errors) ;
   exit 1
  )
  else
    exit 0
