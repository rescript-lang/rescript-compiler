(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1995 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

external mycallback1 : ('a -> 'b) -> 'a -> 'b = "mycallback1"
external mycallback2 : ('a -> 'b -> 'c) -> 'a -> 'b -> 'c = "mycallback2"
external mycallback3 : ('a -> 'b -> 'c -> 'd) -> 'a -> 'b -> 'c -> 'd
    = "mycallback3"
external mycallback4 :
    ('a -> 'b -> 'c -> 'd -> 'e) -> 'a -> 'b -> 'c -> 'd -> 'e = "mycallback4"

let rec tak (x, y, z as _tuple) =
  if x > y then tak(tak (x-1, y, z), tak (y-1, z, x), tak (z-1, x, y))
           else z

let tak2 x (y, z) = tak (x, y, z)

let tak3 x y z = tak (x, y, z)

let tak4 x y z u = tak (x, y, z + u)

let raise_exit () = (raise Exit : unit)

let trapexit () =
  begin try
    mycallback1 raise_exit ()
  with Exit ->
    ()
  end;
  tak (18, 12, 6)

external mypushroot : 'a -> ('b -> 'c) -> 'b -> 'a = "mypushroot"
external mycamlparam : 'a -> ('b -> 'c) -> 'b -> 'a = "mycamlparam"

let tripwire f =
  let s = String.make 5 'a' in
  f s trapexit ()

(* Test callbacks performed to handle signals *)

let sighandler signo =
(*
  print_string "Got signal, triggering garbage collection...";
  print_newline();
*)
  (* Thoroughly wipe the minor heap *)
  ignore (tak (18, 12, 6))

external unix_getpid : unit -> int = "unix_getpid" "noalloc"
external unix_kill : int -> int -> unit = "unix_kill" "noalloc"

let callbacksig () =
  let pid = unix_getpid() in
  (* Allocate a block in the minor heap *)
  let s = String.make 5 'b' in
  (* Send a signal to self.  We want s to remain in a register and
     not be spilled on the stack, hence we declare unix_kill
     "noalloc". *)
  unix_kill pid Sys.sigusr1;
  (* Allocate some more so that the signal will be tested *)
  let u = (s, s) in
  fst u

let _ =
  print_int(mycallback1 tak (18, 12, 6)); print_newline();
  print_int(mycallback2 tak2 18 (12, 6)); print_newline();
  print_int(mycallback3 tak3 18 12 6); print_newline();
  print_int(mycallback4 tak4 18 12 3 3); print_newline();
  print_int(trapexit ()); print_newline();
  print_string(tripwire mypushroot); print_newline();
  print_string(tripwire mycamlparam); print_newline();
  Sys.set_signal Sys.sigusr1 (Sys.Signal_handle sighandler);
  print_string(callbacksig ()); print_newline()
