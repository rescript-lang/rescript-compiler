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

let test n check res =
  print_string "Test "; print_int n;
  if check res then print_string " passed.\n" else print_string " FAILED.\n";
  flush stderr

let eq0 = function 0 -> true | _ -> false
let eqm1 = function -1 -> true | _ -> false
let eq1 = function 1 -> true | _ -> false
let eqtrue (b:bool) = b
let eqftffff =
  function (false,true,false,false,false,false) -> true | _ -> false

let x = [1;2;3]

let f x = 1 :: 2 :: 3 :: x

let mklist len =
  let l = ref [] in
  for i = 1 to len do l := i :: !l done;
  !l

type tree = Dummy | Leaf | Node of tree * tree

let rec mktree depth =
  if depth <= 0 then Leaf else Node(mktree(depth - 1), mktree(depth - 1))

type 'a leftlist = Nil | Cons of 'a leftlist * 'a

let mkleftlist len =
  let l = ref Nil in
  for i = 1 to len do l := Cons(!l, i) done;
  !l

let _ =
  test 1 eq0 (compare 0 0);
  test 2 eqm1 (compare 0 1);
  test 3 eq1 (compare 1 0);
  test 4 eq0 (compare max_int max_int);
  test 5 eqm1 (compare min_int max_int);
  test 6 eq1 (compare max_int min_int);
  test 7 eq0 (compare "foo" "foo");
  test 8 eqm1 (compare "foo" "zorglub");
  test 9 eqm1 (compare "abcdef" "foo");
  test 10 eqm1 (compare "abcdefghij" "abcdefghijkl");
  test 11 eq1 (compare "abcdefghij" "abcdefghi");
  test 12 eq0 (compare (0,1) (0,1));
  test 13 eqm1 (compare (0,1) (0,2));
  test 14 eqm1 (compare (0,1) (1,0));
  test 15 eq1 (compare (0,1) (0,0));
  test 16 eq1 (compare (1,0) (0,1));
  test 17 eq0 (compare 0.0 0.0);
  test 18 eqm1 (compare 0.0 1.0);
  test 19 eqm1 (compare (-1.0) 0.0);
  test 20 eq0 (compare [| 0.0; 1.0; 2.0 |] [| 0.0; 1.0; 2.0 |]);
  test 21 eqm1 (compare [| 0.0; 1.0; 2.0 |] [| 0.0; 1.0; 3.0 |]);
  test 22 eq1 (compare [| 0.0; 5.0; 2.0 |] [| 0.0; 1.0; 2.0 |]);
  test 23 eq0 (compare [1;2;3;4] [1;2;3;4]);
  test 24 eqm1 (compare [1;2;3;4] [1;2;5;6]);
  test 25 eqm1 (compare [1;2;3;4] [1;2;3;4;5]);
  test 26 eq1 (compare [1;2;3;4] [1;2;3]);
  test 27 eq1 (compare [1;2;3;4] [1;2;0;4]);
  test 28 eq0 (compare (mklist 1000) (mklist 1000));
  test 29 eq0 (compare (mkleftlist 1000) (mkleftlist 1000));
  test 30 eq0 (compare (mktree 12) (mktree 12));
  test 31 eqtrue (x = f []);
  test 32 eqtrue (stdout <> stderr);
  test 33 eqm1 (compare nan 0.0);
  test 34 eqm1 (compare nan neg_infinity);
  test 35 eq0 (compare nan nan);
  test 36 eqm1 (compare (0.0, nan) (0.0, 0.0));
  test 37 eqm1 (compare (0.0, nan) (0.0, neg_infinity));
  test 38 eq0 (compare (nan, 0.0) (nan, 0.0));
  let cmpgen x y = (x=y, x<>y, x<y, x<=y, x>y, x>=y) in
  let cmpfloat (x:float) (y:float) = (x=y, x<>y, x<y, x<=y, x>y, x>=y) in
  test 39 eqftffff (cmpgen nan nan);
  test 40 eqftffff (cmpgen nan 0.0);
  test 41 eqftffff (cmpfloat nan nan);
  test 42 eqftffff (cmpfloat nan 0.0);
  test 43 eqtrue ([||] = [||]);
  (* Convoluted forms to test both the "positive" and "negative" cases
     of float tests *)
  let cmpfloatpos (x:float) (y:float) =
    ((let r = ref false in (if x = y then r := true); !r),
     (let r = ref false in (if x <> y then r := true); !r),
     (let r = ref false in (if x < y then r := true); !r),
     (let r = ref false in (if x <= y then r := true); !r),
     (let r = ref false in (if x > y then r := true); !r),
     (let r = ref false in (if x >= y then r := true); !r))
  and cmpfloatneg (x:float) (y:float) =
    ((let r = ref true in (if not (x = y) then r := false); !r),
     (let r = ref true in (if not (x <> y) then r := false); !r),
     (let r = ref true in (if not (x < y) then r := false); !r),
     (let r = ref true in (if not (x <= y) then r := false); !r),
     (let r = ref true in (if not (x > y) then r := false); !r),
     (let r = ref true in (if not (x >= y) then r := false); !r)) in
  let testcmpfloat x y =
    cmpfloatpos x y = cmpgen x y &&
    cmpfloatneg x y = cmpgen x y in
  test 50 eqtrue (testcmpfloat nan nan);
  test 51 eqtrue (testcmpfloat nan 0.0);
  test 52 eqtrue (testcmpfloat 0.0 nan);
  test 53 eqtrue (testcmpfloat 0.0 0.0);
  test 54 eqtrue (testcmpfloat 1.0 0.0);
  test 55 eqtrue (testcmpfloat 0.0 1.0)
