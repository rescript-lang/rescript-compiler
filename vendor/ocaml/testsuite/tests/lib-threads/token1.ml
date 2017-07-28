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

(* Performance test for mutexes and conditions *)

let mut = Mutex.create()

let niter = ref 0

let token = ref 0

let process (n, conds, nprocs) =
  while true do
    Mutex.lock mut;
    while !token <> n do
      (* Printf.printf "Thread %d waiting (token = %d)\n" n !token; *)
      Condition.wait conds.(n) mut
    done;
    (* Printf.printf "Thread %d got token %d\n" n !token; *)
    incr token;
    if !token >= nprocs then token := 0;
    if n = 0 then begin
      decr niter;
      if !niter <= 0 then exit 0
    end;
    Condition.signal conds.(!token);
    Mutex.unlock mut
  done

let main() =
  let nprocs = try int_of_string Sys.argv.(1) with _ -> 100 in
  let iter = try int_of_string Sys.argv.(2) with _ -> 1000 in
  let conds = Array.make nprocs (Condition.create()) in
  for i = 1 to nprocs - 1 do conds.(i) <- Condition.create() done;
  niter := iter;
  for i = 0 to nprocs - 1 do Thread.create process (i, conds, nprocs) done;
  Thread.delay 3600.

let _ = main()
