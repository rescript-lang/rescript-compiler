(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*             Pierre Weis, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2006 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open Testing;;

open Printf;;

(* Padding floating point numbers.
   Testing * width specifications. *)
let test0 () =
  sprintf "%.0f" 1.0 = "1" &&
  sprintf "%.0f." 1.7 = "2." &&
  sprintf "%.1f." 1.0 = "1.0." &&
  (*sprintf "%0.1f." 12.0 = "12.0." &&*)
    (* >> '0' w/o padding *)
  sprintf "%3.1f." 12.0 = "12.0." &&
  sprintf "%5.1f." 12.0 = " 12.0." &&
  sprintf "%10.1f." 12.0 = "      12.0." &&
  sprintf "%010.1f." 12.0 = "00000012.0." &&
  sprintf "% 10.1f." 12.0 = "      12.0." &&
  sprintf "%+10.1f." 12.0 = "     +12.0." &&
  sprintf "%+10.1f." (-12.0) = "     -12.0." &&

  sprintf "%010.5f." 12.0 = "0012.00000." &&
  sprintf "%010.0f." 12.0 = "0000000012." &&
  sprintf "% 10.0f." 12.0 = "        12." &&

  (*sprintf "%0.1f." 12.0 = "12.0." &&*)
    (* >> '0' w/o padding *)
  sprintf "%10.1f." 1.001 = "       1.0." &&
  sprintf "%05.1f." 1.001 = "001.0."
;;

test (test0 ());;

(* Padding integers (cf bug 3955).
   Testing * width specifications. *)
let test1 () =
  sprintf "%d\n" 1 = "1\n" &&
  sprintf "%05d\n" 1 = "00001\n" &&
  sprintf "%*d\n" 5 1 = "    1\n" &&
  sprintf "%0*d\n" 5 1 = "00001\n";;

test (test1 ());;

(* FIXME: when positional specification will be OK. *)
let test2 () = true
(*  sprintf "%1$d\n" 5 1 = "    1\n" &&
  sprintf "%01$d\n" 5 1 = "00001\n" *);;

test (test2 ());;

(* Testing meta format string printing. *)
let test3 () =
  sprintf "%{toto %S titi.\n%}" "Bonjour %S." = "%s" &&
  sprintf "%{Bonjour %S.%}" "toto %S titi.\n" = "%s"
;;
test (test3 ());;

(* Testing meta format string arguments. *)
let test4 () =
  sprintf "%(%s%)" "Bonjour %s" "toto" = "Bonjour toto" &&
  sprintf "%(%s%)" "Bonjour %s." "vous" = "Bonjour vous." &&
  sprintf "%(%s%)" "Hello %s." "you" = "Hello you."
;;

test (test4 ());;

let test5 () =
  sprintf "%(toto %s titi.\n%)"
    "Bonjour %s." "vous" = "Bonjour vous." &&
  sprintf "%(toto %s titi.\n%).\n"
    "Bonjour %s" "toto" = "Bonjour toto.\n" &&
  sprintf "%(toto %s titi.\n%)%s\n"
    "Bonjour %s." "toto" " Ca va?" = "Bonjour toto. Ca va?\n"
;;

test (test5 ());;
