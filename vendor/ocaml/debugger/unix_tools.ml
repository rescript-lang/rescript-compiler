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

(****************** Tools for Unix *************************************)

open Misc
open Unix

(*** Convert a socket name into a socket address. ***)
let convert_address address =
  try
    let n = String.index address ':' in
      let host = String.sub address 0 n
      and port = String.sub address (n + 1) (String.length address - n - 1)
      in
        (PF_INET,
         ADDR_INET
           ((try inet_addr_of_string host with Failure _ ->
               try (gethostbyname host).h_addr_list.(0) with Not_found ->
                 prerr_endline ("Unknown host: " ^ host);
                 failwith "Can't convert address"),
            (try int_of_string port with Failure _ ->
               prerr_endline "The port number should be an integer";
               failwith "Can't convert address")))
  with Not_found ->
    match Sys.os_type with
      "Win32" -> failwith "Unix sockets not supported"
    | _ -> (PF_UNIX, ADDR_UNIX address)

(*** Report a unix error. ***)
let report_error = function
  | Unix_error (err, fun_name, arg) ->
     prerr_string "Unix error: '";
     prerr_string fun_name;
     prerr_string "' failed";
     if String.length arg > 0 then
       (prerr_string " on '";
        prerr_string arg;
        prerr_string "'");
     prerr_string ": ";
     prerr_endline (error_message err)
  | _ -> fatal_error "report_error: not a Unix error"

(* Find program `name' in `PATH'. *)
(* Return the full path if found. *)
(* Raise `Not_found' otherwise. *)
let search_in_path name =
  Printf.fprintf Pervasives.stderr "search_in_path [%s]\n%!" name;
  let check name =
    try access name [X_OK]; name with Unix_error _ -> raise Not_found
  in
    if not (Filename.is_implicit name) then
      check name
    else
      let path = Sys.getenv "PATH" in
        let length = String.length path in
          let rec traverse pointer =
            if (pointer >= length) || (path.[pointer] = ':') then
              pointer
            else
              traverse (pointer + 1)
          in
            let rec find pos =
              let pos2 = traverse pos in
                let directory = (String.sub path pos (pos2 - pos)) in
                  let fullname =
                    if directory = "" then name else directory ^ "/" ^ name
                  in
                    try check fullname with
                    | Not_found ->
                        if pos2 < length then find (pos2 + 1)
                        else raise Not_found
          in
            find 0

(* Expand a path. *)
(* ### path -> path' *)
let rec expand_path ch =
  let rec subst_variable ch =
    try
      let pos = String.index ch '$' in
        if (pos + 1 < String.length ch) && (ch.[pos + 1] = '$') then
          (String.sub ch 0 (pos + 1))
            ^ (subst_variable
                 (String.sub ch (pos + 2) (String.length ch - pos - 2)))
        else
          (String.sub ch 0 pos)
            ^ (subst2 (String.sub ch (pos + 1) (String.length ch - pos - 1)))
    with Not_found ->
      ch
  and subst2 ch =
    let suiv =
      let i = ref 0 in
        while !i < String.length ch &&
              (let c = ch.[!i] in (c >= 'a' && c <= 'z')
                               || (c >= 'A' && c <= 'Z')
                               || (c >= '0' && c <= '9')
                               || c = '_')
        do incr i done;
        !i
    in (Sys.getenv (String.sub ch 0 suiv))
       ^ (subst_variable (String.sub ch suiv (String.length ch - suiv)))
  in
    let ch = subst_variable ch in
      let concat_root nom ch2 =
        try Filename.concat (getpwnam nom).pw_dir ch2
        with Not_found ->
          "~" ^ nom
      in
        if ch.[0] = '~' then
          try
            match String.index ch '/' with
              1 ->
                (let tail = String.sub ch 2 (String.length ch - 2)
                 in
                   try Filename.concat (Sys.getenv "HOME") tail
                   with Not_found ->
                     concat_root (Sys.getenv "LOGNAME") tail)
            |  n -> concat_root
                      (String.sub ch 1 (n - 1))
                      (String.sub ch (n + 1) (String.length ch - n - 1))
          with
            Not_found ->
              expand_path (ch ^ "/")
        else ch

let make_absolute name =
  if Filename.is_relative name
  then Filename.concat (getcwd ()) name
  else name
;;
