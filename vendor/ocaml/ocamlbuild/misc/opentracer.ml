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


open My_std

module type TRACER = sig
  (** Call the given command using the tracer, it returns the exit status. *)
  val call : string -> string list -> StringSet.t * Unix.process_status
end

module Ktrace = struct
  let process_line line (wait_a_string, set) =
    let strings = Lexers.space_sep_strings (Lexing.from_string line) in
    if wait_a_string then
      match strings with
      | [_; _; "NAMI"; file] -> false, StringSet.add file set
      | _ -> failwith (Printf.sprintf "unexpected ktrace output line (%S)" line)
    else
      match strings with
      | [_; _; "CALL"; fct] ->
          (String.length fct > 5 && String.sub fct 0 5 = "open("), set
      | _ -> false, set

  let call cmd args =
    let tmp = Filename.temp_file "ktrace" "out" in
    match Unix.fork () with
    | 0 -> Unix.execvp "ktrace" (Array.of_list("-d"::"-i"::"-t"::"nc"::"-f"::tmp::cmd::args))
    | pid ->
        let _, st = Unix.waitpid [] pid in
        let ic = Unix.open_process_in (Printf.sprintf "kdump -f %s" (Filename.quote tmp)) in
        let close () = ignore (Unix.close_process_in ic); Sys.remove tmp in
        let set =
          try
            let rec loop acc =
              match try Some (input_line ic) with End_of_file -> None with
              | Some line -> loop (process_line line acc)
              | None -> acc in
            let _, set = loop (false, StringSet.empty) in
            close ();
            set
          with e -> (close (); raise e)
        in set, st

end

module Driver (T : TRACER) = struct
  let usage () =
    Printf.eprintf "Usage: %s [-a <authorized_file>]* <cmd> <args>*\n%!" Sys.argv.(0);
    exit 2

  let main () =
    let log = "opentracer.log" in
    let oc =
      if sys_file_exists log then
        open_out_gen [Open_wronly;Open_append;Open_text] 0 log
      else
        let oc = open_out log in
        let () = output_string oc "---\n" in
        oc in
    let rec loop acc =
      function
      | "-a" :: file :: rest -> loop (StringSet.add file acc) rest
      | "-a" :: _ -> usage ()
      | "--" :: cmd :: args -> acc, cmd, args
      | cmd :: args -> acc, cmd, args
      | [] -> usage () in
    let authorized_files, cmd, args =
      loop StringSet.empty (List.tl (Array.to_list Sys.argv)) in
    let opened_files, st = T.call cmd args in
    let forbidden_files = StringSet.diff opened_files authorized_files in

    if not (StringSet.is_empty forbidden_files) then begin
      Printf.fprintf oc "- cmd: %s\n  args:\n%!" cmd;
      let pp = Printf.fprintf oc "    - %s\n%!" in
      List.iter pp args;
      Printf.fprintf oc "  forbidden_files:\n%!";
      StringSet.iter pp forbidden_files;
    end;
    close_out oc;
    match st with
    | Unix.WEXITED st -> exit st
    | Unix.WSIGNALED s | Unix.WSTOPPED s -> Unix.kill (Unix.getpid ()) s
end

let main =
  (* match os with *)
  (* | "macos" -> *)
    let module M = Driver(Ktrace) in M.main
  (* | "linux" -> *)
    (* let module M = Driver(Strace) in M.main *)

let () = main ()
