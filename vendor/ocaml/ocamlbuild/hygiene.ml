(***********************************************************************)
(*                                                                     *)
(*                             ocamlbuild                              *)
(*                                                                     *)
(*  Nicolas Pouillard, Berke Durak, projet Gallium, INRIA Rocquencourt *)
(*                                                                     *)
(*  Copyright 2007 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)


(* Original author: Berke Durak *)
(* Hygiene *)
open My_std
open Slurp

exception Exit_hygiene_violations

type rule =
| Implies_not of pattern * pattern
| Not of pattern
and pattern = suffix
and suffix = string

type penalty = Warn | Fail

type law = {
  law_name : string;
  law_rules : rule list;
  law_penalty : penalty
}

let list_collect f l =
  let rec loop result = function
    | [] -> List.rev result
    | x :: rest ->
        match f x with
        | None -> loop result rest
        | Some y -> loop (y :: result) rest
  in
  loop [] l

let list_none_for_all f l =
  let rec loop = function
    | [] -> None
    | x :: rest ->
        match f x with
        | None -> loop rest
        | y -> y
  in
  loop l

let sf = Printf.sprintf

module SS = Set.Make(String);;

let check ?sanitize laws entry =
  let penalties = ref [] in
  let microbes = ref SS.empty in
  let () =
    match sanitize with
    | Some fn -> if sys_file_exists fn then sys_remove fn
    | None -> ()
  in
  let remove path name =
    if sanitize <> None then
      microbes := SS.add (filename_concat path name) !microbes
  in
  let check_rule = fun entries -> function
    | Not suffix ->
        list_collect
          begin function
            | File(path, name, _, true) ->
                if Filename.check_suffix name suffix
                  && not ( Pathname.link_to_dir (filename_concat path name) !Options.build_dir ) then
                  begin
                    remove path name;
                    Some(sf "File %s in %s has suffix %s" name path suffix)
                  end
                else
                  None
            | File _ | Dir _| Error _ | Nothing -> None
          end
          entries
    | Implies_not(suffix1, suffix2) ->
        list_collect
          begin function
            | File(path, name, _, true) ->
                if Filename.check_suffix name suffix1 then
                  begin
                    let base = Filename.chop_suffix name suffix1 in
                    let name' = base ^ suffix2 in
                    if List.exists
                       begin function
                         | File(_, name'', _, true) -> name' = name''
                         | File _ | Dir _ | Error _ | Nothing -> false
                       end
                       entries
                    then
                      begin
                        remove path name';
                        Some(sf "Files %s and %s should not be together in %s" name name' path)
                      end
                    else
                      None
                  end
                else
                  None
            | File _ | Dir _ | Error _ | Nothing -> None
          end
          entries
  in
  let rec check_entry = function
    | Dir(_,_,_,true,entries) ->
        List.iter
          begin fun law ->
            match List.concat (List.map (check_rule !*entries) law.law_rules) with
            | [] -> ()
            | explanations ->
              penalties := (law, explanations) :: !penalties
          end
          laws;
        List.iter check_entry !*entries
    | Dir _ | File _ | Error _ | Nothing -> ()
  in
  check_entry entry;
  begin
    let microbes = !microbes in
    if not (SS.is_empty microbes) then
      begin
        match sanitize with
        | None ->
            Log.eprintf "sanitize: the following are files that should probably not be in your\n\
                         source tree:\n";
            SS.iter
              begin fun fn ->
                Log.eprintf " %s" fn
              end
              microbes;
            Log.eprintf "Remove them manually, don't use the -no-sanitize option, use -no-hygiene, or\n\
                          define hygiene exceptions using the tags or plugin mechanism.\n";
            raise Exit_hygiene_violations
        | Some fn ->
            let m = SS.cardinal microbes in
            Log.eprintf
              "@[<hov 2>SANITIZE:@ a@ total@ of@ %d@ file%s@ that@ should@ probably\
               @ not@ be@ in@ your@ source@ tree@ has@ been@ found.\
               @ A@ script@ shell@ file@ %S@ is@ being@ created.\
               @ Check@ this@ script@ and@ run@ it@ to@ remove@ unwanted@ files\
               @ or@ use@ other@ options@ (such@ as@ defining@ hygiene@ exceptions\
               @ or@ using@ the@ -no-hygiene@ option).@]"
               m (if m = 1 then "" else "s") fn;
            let oc = open_out_gen [Open_wronly; Open_creat; Open_trunc; Open_binary] 0o777 fn in
            (* See PR #5338: under mingw, one produces a shell script, which must follow
               Unix eol convention; hence Open_binary. *)
            let fp = Printf.fprintf in
            fp oc "#!/bin/sh\n\
                   # File generated by ocamlbuild\n\
                   \n\
                   cd %s\n\
                   \n" (Shell.quote_filename_if_needed Pathname.pwd);
            SS.iter
              begin fun fn ->
                fp oc "rm -f %s\n" (Shell.quote_filename_if_needed fn)
              end
              microbes;
            (* Also clean itself *)
            fp oc "# Also clean the script itself\n";
            fp oc "rm -f %s\n" (Shell.quote_filename_if_needed fn);
            close_out oc
      end;
    !penalties
  end
;;
