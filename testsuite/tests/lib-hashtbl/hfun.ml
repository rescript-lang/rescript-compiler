(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Gallium, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2011 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Testing the hash function Hashtbl.hash *)
(* What is tested:
     - reproducibility on various platforms, esp. 32/64 bit issues
     - equal values hash equally, esp NaNs. *)

open Printf

let _ =
  printf "-- Strings:\n";
  printf "\"\"\t\t%08x\n" (Hashtbl.hash "");
  printf "\"Hello world\"\t%08x\n" (Hashtbl.hash "Hello world");

  printf "-- Integers:\n";
  printf "0\t\t%08x\n" (Hashtbl.hash 0);
  printf "-1\t\t%08x\n" (Hashtbl.hash (-1));
  printf "42\t\t%08x\n" (Hashtbl.hash 42);
  printf "2^30-1\t\t%08x\n" (Hashtbl.hash 0x3FFF_FFFF);
  printf "-2^30\t\t%08x\n" (Hashtbl.hash (-0x4000_0000));

  printf "-- Floats:\n";
  printf "+0.0\t\t%08x\n" (Hashtbl.hash 0.0);
  printf "-0.0\t\t%08x\n" (Hashtbl.hash (-. 0.0));
  printf "+infty\t\t%08x\n" (Hashtbl.hash infinity);
  printf "-infty\t\t%08x\n" (Hashtbl.hash neg_infinity);
  printf "NaN\t\t%08x\n" (Hashtbl.hash nan);
  printf "NaN#2\t\t%08x\n"
         (Hashtbl.hash (Int64.float_of_bits 0xFF_F0_00_12_34_56_78_9AL));
  printf "NaN#3\t\t%08x\n" (Hashtbl.hash (0.0 /. 0.0));

  printf "-- Native integers:\n";
  printf "0\t\t%08x\n" (Hashtbl.hash 0n);
  printf "-1\t\t%08x\n" (Hashtbl.hash (-1n));
  printf "42\t\t%08x\n" (Hashtbl.hash 42n);
  printf "2^30-1\t\t%08x\n" (Hashtbl.hash 0x3FFF_FFFFn);
  printf "-2^30\t\t%08x\n" (Hashtbl.hash (-0x4000_0000n));

  printf "-- Lists:\n";
  printf "[0..10]\t\t%08x\n" (Hashtbl.hash [0;1;2;3;4;5;6;7;8;9;10]);
  printf "[0..12]\t\t%08x\n" (Hashtbl.hash [0;1;2;3;4;5;6;7;8;9;10;11;12]);
  printf "[10..0]\t\t%08x\n" (Hashtbl.hash [10;9;8;7;6;5;4;3;2;1;0]);

  ()
