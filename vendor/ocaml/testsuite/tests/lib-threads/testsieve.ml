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

let sieve primes=
  Event.sync (Event.send primes 0);
  Event.sync (Event.send primes 1);
  Event.sync (Event.send primes 2);
  let integers = Event.new_channel () in
  let rec enumerate n=
    Event.sync (Event.send integers n);
    enumerate (n + 2)
  and filter inpout =
    let n = Event.sync (Event.receive inpout)
    (* On prepare le terrain pour l'appel recursif *)
    and output = Event.new_channel () in
    (* Celui qui etait en tete du crible est premier *)
    Event.sync (Event.send primes n);
    Thread.create filter output;
    (* On elimine de la sortie ceux qui sont des multiples de n *)
    while true do
        let m = Event.sync (Event.receive inpout) in
        (* print_int n; print_string ": "; print_int m; print_newline(); *)
        if (m mod n) = 0
        then ()
        else ((Event.sync (Event.send output m));())
      done in
  Thread.create filter integers;
  Thread.create enumerate 3

let premiers = Event.new_channel ()

let main _ =
  Thread.create sieve premiers;
  while true do
    for i = 1 to 100 do
      let n = Event.sync (Event.receive premiers) in
      print_int n; print_newline()
    done;
    exit 0
    done


let _ =
  try main ()
  with _ -> exit 0;;
