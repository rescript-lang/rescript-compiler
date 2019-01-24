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

(* Handling of keyboard interrupts *)

let interrupted = ref false

let is_protected = ref false

let break _signum =
  if !is_protected
  then interrupted := true
  else raise Sys.Break

let _ =
  match Sys.os_type with
    "Win32" -> ()
  | _ ->
      Sys.set_signal Sys.sigint (Sys.Signal_handle break);
      Sys.set_signal Sys.sigpipe (Sys.Signal_handle(fun _ -> raise End_of_file))

let protect f =
  if !is_protected then
    f ()
  else begin
    is_protected := true;
    if not !interrupted then
       f ();
    is_protected := false;
    if !interrupted then begin interrupted := false; raise Sys.Break end
  end

let unprotect f =
  if not !is_protected then
    f ()
  else begin
    is_protected := false;
    if !interrupted then begin interrupted := false; raise Sys.Break end;
    f ();
    is_protected := true
  end
