(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Performance test for I/O scheduling *)

let mut = Mutex.create()

let niter = ref 0

let token = ref 0

let process (n, ins, outs, nprocs) =
  let buf = String.make 1 '.' in
  while buf <> "-" do
    Unix.read ins.(n) buf 0 1;
    (* Printf.printf "Thread %d got the token\n" n; *)
    if n = 0 then begin
      decr niter;
      if !niter <= 0 then buf.[0] <- '-';
    end;
    let next = if n + 1 >= nprocs then 0 else n + 1 in
    (* Printf.printf "Thread %d sending token to thread %d\n" n next; *)
    Unix.write outs.(next) buf 0 1
  done

let main() =
  let nprocs = try int_of_string Sys.argv.(1) with _ -> 100 in
  let iter = try int_of_string Sys.argv.(2) with _ -> 1000 in
  let ins = Array.make nprocs Unix.stdin in
  let outs = Array.make nprocs Unix.stdout in
  let threads = Array.make nprocs (Thread.self ()) in
  for n = 0 to nprocs - 1 do
    let (i, o) = Unix.pipe() in ins.(n) <- i; outs.(n) <- o
  done;
  niter := iter;
  for i = 0 to nprocs - 1 do
    threads.(i) <- Thread.create process (i, ins, outs, nprocs)
  done;
  Unix.write outs.(0) "X" 0 1;
  for i = 0 to nprocs - 1 do Thread.join threads.(i) done

let _ = main()
