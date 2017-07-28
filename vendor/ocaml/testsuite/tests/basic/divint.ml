(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Gallium, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2013 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open Printf

(* Test integer division and modulus, esp. ocamlopt's optimization
   when the divisor is a constant. *)

let error = ref false

module WithInt = struct

let d = ref 0
let divref n = n / !d
let modref n = n mod !d

let test_one (df: int -> int) (mf: int -> int) x =
  if not (df x = divref x && mf x = modref x) then begin
    printf "mismatch for %d\n" x;
    error := true
  end

let do_test divisor (df: int -> int) (mf: int -> int) =
  d := divisor;
  List.iter (test_one df mf)
    [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10;
     100; 1000; 10000; 100000; 1000000; max_int - 1; max_int;
     -1; -2; -3; -4; -5; -6; -7; -8; -9; -10;
     -100; -1000; -10000; -100000; -1000000; min_int + 1; min_int];
  let seed = ref 0 in
  for i = 1 to 1000 do
    seed := !seed * 69069 + 25173;
    test_one df mf !seed
  done

end

module WithNat = struct

let d = ref 0n
let divref n = Nativeint.div n !d
let modref n = Nativeint.rem n !d

let test_one (df: nativeint -> nativeint) (mf: nativeint -> nativeint) x =
  if not (df x = divref x && mf x = modref x) then begin
    printf "mismatch for %nd\n" x;
    error := true
  end

let do_test divisor (df: nativeint -> nativeint) (mf: nativeint -> nativeint) =
  d := Nativeint.of_int divisor;
  List.iter (test_one df mf)
    [0n; 1n; 2n; 3n; 4n; 5n; 6n; 7n; 8n; 9n; 10n;
     100n; 1000n; 10000n; 100000n; 1000000n;
     Nativeint.(pred max_int); Nativeint.max_int;
     -1n; -2n; -3n; -4n; -5n; -6n; -7n; -8n; -9n; -10n;
     -100n; -1000n; -10000n; -100000n; -1000000n;
     Nativeint.(succ min_int); Nativeint.min_int];
  let seed = ref 0n in
  for i = 1 to 1000 do
    seed := Nativeint.(add (mul !seed 69069n) 25173n);
    test_one df mf !seed
  done

end

let _ =
  printf "1 int\n"; WithInt.do_test 1 (fun x -> x / 1)(fun x -> x mod 1);
  printf "2 int\n"; WithInt.do_test 2 (fun x -> x / 2)(fun x -> x mod 2);
  printf "3 int\n"; WithInt.do_test 3 (fun x -> x / 3)(fun x -> x mod 3);
  printf "4 int\n"; WithInt.do_test 4 (fun x -> x / 4)(fun x -> x mod 4);
  printf "5 int\n"; WithInt.do_test 5 (fun x -> x / 5)(fun x -> x mod 5);
  printf "6 int\n"; WithInt.do_test 6 (fun x -> x / 6)(fun x -> x mod 6);
  printf "7 int\n"; WithInt.do_test 7 (fun x -> x / 7)(fun x -> x mod 7);
  printf "9 int\n"; WithInt.do_test 9 (fun x -> x / 9)(fun x -> x mod 9);
  printf "10 int\n"; WithInt.do_test 10 (fun x -> x / 10)(fun x -> x mod 10);
  printf "11 int\n"; WithInt.do_test 11 (fun x -> x / 11)(fun x -> x mod 11);
  printf "12 int\n"; WithInt.do_test 12 (fun x -> x / 12)(fun x -> x mod 12);
  printf "25 int\n"; WithInt.do_test 25 (fun x -> x / 25)(fun x -> x mod 25);
  printf "55 int\n"; WithInt.do_test 55 (fun x -> x / 55)(fun x -> x mod 55);
  printf "125 int\n"; WithInt.do_test 125 (fun x -> x / 125)(fun x -> x mod 125);
  printf "625 int\n"; WithInt.do_test 625 (fun x -> x / 625)(fun x -> x mod 625);
  printf "-1 int\n"; WithInt.do_test (-1) (fun x -> x / (-1))(fun x -> x mod (-1));
  printf "-2 int\n"; WithInt.do_test (-2) (fun x -> x / (-2))(fun x -> x mod (-2));
  printf "-3 int\n"; WithInt.do_test (-3) (fun x -> x / (-3))(fun x -> x mod (-3));

  printf "1 nat\n"; WithNat.do_test 1 (fun x -> Nativeint.div x 1n)(fun x -> Nativeint.rem x 1n);
  printf "2 nat\n"; WithNat.do_test 2 (fun x -> Nativeint.div x 2n)(fun x -> Nativeint.rem x 2n);
  printf "3 nat\n"; WithNat.do_test 3 (fun x -> Nativeint.div x 3n)(fun x -> Nativeint.rem x 3n);
  printf "4 nat\n"; WithNat.do_test 4 (fun x -> Nativeint.div x 4n)(fun x -> Nativeint.rem x 4n);
  printf "5 nat\n"; WithNat.do_test 5 (fun x -> Nativeint.div x 5n)(fun x -> Nativeint.rem x 5n);
  printf "6 nat\n"; WithNat.do_test 6 (fun x -> Nativeint.div x 6n)(fun x -> Nativeint.rem x 6n);
  printf "7 nat\n"; WithNat.do_test 7 (fun x -> Nativeint.div x 7n)(fun x -> Nativeint.rem x 7n);
  printf "9 nat\n"; WithNat.do_test 9 (fun x -> Nativeint.div x 9n)(fun x -> Nativeint.rem x 9n);
  printf "10 nat\n"; WithNat.do_test 10 (fun x -> Nativeint.div x 10n)(fun x -> Nativeint.rem x 10n);
  printf "11 nat\n"; WithNat.do_test 11 (fun x -> Nativeint.div x 11n)(fun x -> Nativeint.rem x 11n);
  printf "12 nat\n"; WithNat.do_test 12 (fun x -> Nativeint.div x 12n)(fun x -> Nativeint.rem x 12n);
  printf "25 nat\n"; WithNat.do_test 25 (fun x -> Nativeint.div x 25n)(fun x -> Nativeint.rem x 25n);
  printf "55 nat\n"; WithNat.do_test 55 (fun x -> Nativeint.div x 55n)(fun x -> Nativeint.rem x 55n);
  printf "125 nat\n"; WithNat.do_test 125 (fun x -> Nativeint.div x 125n)(fun x -> Nativeint.rem x 125n);
  printf "625 nat\n"; WithNat.do_test 625 (fun x -> Nativeint.div x 625n)(fun x -> Nativeint.rem x 625n);
  printf "-1 nat\n"; WithNat.do_test (-1) (fun x -> Nativeint.div x (-1n))(fun x -> Nativeint.rem x (-1n));
  printf "-2 nat\n"; WithNat.do_test (-2) (fun x -> Nativeint.div x (-2n))(fun x -> Nativeint.rem x (-2n));
  printf "-3 nat\n"; WithNat.do_test (-3) (fun x -> Nativeint.div x (-3n))(fun x -> Nativeint.rem x (-3n));

  if !error then printf "TEST FAILED.\n" else printf "Test passed.\n"

(* PR#6879 *)
let f n = assert (1 mod n = 0)
let () = f 1
