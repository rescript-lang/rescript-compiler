(***********************************************************************)
(* *)
(* OCaml *)
(* *)
(* Xavier Leroy, projet Gallium, INRIA Rocquencourt *)
(* *)
(* Copyright 2014 Institut National de Recherche en Informatique et *)
(* en Automatique. All rights reserved. This file is distributed *)
(* under the terms of the Q Public License version 1.0. *)
(* *)
(***********************************************************************)

(* Test constant propagation through inlining *)

(* constprop.ml is generated from constprop.mlp using
      cpp constprop.mlp > constprop.ml
*)
let do_test msg res1 res2 =
  Printf.printf "%s: %s\n" msg (if res1 = res2 then "passed" else "FAILED")
(* Hide a constant from the optimizer, preventing constant propagation *)
let hide x = List.nth [x] 0
let _ =
  begin
    let x = true and y = false in
    let xh = hide x and yh = hide y in
    do_test "booleans" ((x && y, x || y, not x)) ((xh && yh, xh || yh, not xh))
  end;
  begin
    let x = 89809344 and y = 457455773 and s = 7 in
    let xh = hide x and yh = hide y and sh = hide s in
    do_test "integers" ((-x, x + y, x - y, x * y, x / y, x mod y, x land y, x lor y, x lxor y, x lsl s, x lsr s, x asr s, x = y, x <> y, x < y, x <= y, x > y, x >= y, succ x, pred y)) ((-xh, xh + yh, xh - yh, xh * yh, xh / yh, xh mod yh, xh land yh, xh lor yh, xh lxor yh, xh lsl sh, xh lsr sh, xh asr sh, xh = yh, xh <> yh, xh < yh, xh <= yh, xh > yh, xh >= yh, succ xh, pred yh))
  end;
  begin
    let x = 3.141592654 and y = 0.341638588598232096 in
    let xh = hide x and yh = hide y in
    do_test "floats" ((int_of_float x, x +. y, x -. y, x *. y, x /. y, x = y, x <> y, x < y, x <= y, x > y, x >= y)) ((int_of_float xh, xh +. yh, xh -. yh, xh *. yh, xh /. yh, xh = yh, xh <> yh, xh < yh, xh <= yh, xh > yh, xh >= yh))
  end;
  begin
    let x = 781944104l and y = 308219921l and s = 3 in
    let xh = hide x and yh = hide y and sh = hide s in
    do_test "32-bit integers" (Int32.(neg x, add x y, sub x y, mul x y, div x y, rem x y, logand x y, logor x y, logxor x y, shift_left x s, shift_right x s, shift_right_logical x s, x = y, x <> y, x < y, x <= y, x > y, x >= y)) (Int32.(neg xh, add xh yh, sub xh yh, mul xh yh, div xh yh, rem xh yh, logand xh yh, logor xh yh, logxor xh yh, shift_left xh sh, shift_right xh sh, shift_right_logical xh sh, xh = yh, xh <> yh, xh < yh, xh <= yh, xh > yh, xh >= yh))
  end;
  begin
    let x = 1828697041n and y = -521695949n and s = 8 in
    let xh = hide x and yh = hide y and sh = hide s in
    do_test "native integers" (Nativeint.(neg x, add x y, sub x y, mul x y, div x y, rem x y, logand x y, logor x y, logxor x y, shift_left x s, shift_right x s, shift_right_logical x s, x = y, x <> y, x < y, x <= y, x > y, x >= y)) (Nativeint.(neg xh, add xh yh, sub xh yh, mul xh yh, div xh yh, rem xh yh, logand xh yh, logor xh yh, logxor xh yh, shift_left xh sh, shift_right xh sh, shift_right_logical xh sh, xh = yh, xh <> yh, xh < yh, xh <= yh, xh > yh, xh >= yh))
  end;
  begin
    let x = 1511491586921138079L and y = 6677538715441746158L and s = 17 in
    let xh = hide x and yh = hide y and sh = hide s in
    do_test "64-bit integers" (Int64.(neg x, add x y, sub x y, mul x y, div x y, rem x y, logand x y, logor x y, logxor x y, shift_left x s, shift_right x s, shift_right_logical x s, x = y, x <> y, x < y, x <= y, x > y, x >= y)) (Int64.(neg xh, add xh yh, sub xh yh, mul xh yh, div xh yh, rem xh yh, logand xh yh, logor xh yh, logxor xh yh, shift_left xh sh, shift_right xh sh, shift_right_logical xh sh, xh = yh, xh <> yh, xh < yh, xh <= yh, xh > yh, xh >= yh))
  end;
  begin
    let x = 1000807289 in
    let xh = hide x in
    do_test "integer conversions" ((float_of_int x, Int32.of_int x, Nativeint.of_int x, Int64.of_int x)) ((float_of_int xh, Int32.of_int xh, Nativeint.of_int xh, Int64.of_int xh))
  end;
  begin
    let x = 10486393l in
    let xh = hide x in
    do_test "32-bit integer conversions" ((Int32.to_int x, Nativeint.of_int32 x, Int64.of_int32 x)) ((Int32.to_int xh, Nativeint.of_int32 xh, Int64.of_int32 xh))
  end;
  begin
    let x = -131134014n in
    let xh = hide x in
    do_test "native integer conversions" ((Nativeint.to_int x, Nativeint.to_int32 x, Int64.of_nativeint x)) ((Nativeint.to_int xh, Nativeint.to_int32 xh, Int64.of_nativeint xh))
  end;
  begin
    let x = 531871273453404175L in
    let xh = hide x in
    do_test "64-bit integer conversions" ((Int64.to_int x, Int64.to_int32 x, Int64.to_nativeint x)) ((Int64.to_int xh, Int64.to_int32 xh, Int64.to_nativeint xh))
  end
