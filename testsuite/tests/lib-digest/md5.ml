(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2003 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Test int32 arithmetic and optimizations using the MD5 algorithm *)

open Printf

type context =
  { buf: string;
    mutable pos: int;
    mutable a: int32;
    mutable b: int32;
    mutable c: int32;
    mutable d: int32;
    mutable bits: int64 }

let step1 w x y z data s =
  let w =
    Int32.add (Int32.add w data)
              (Int32.logxor z (Int32.logand x (Int32.logxor y z))) in
  Int32.add x
    (Int32.logor (Int32.shift_left w s) (Int32.shift_right_logical w (32-s)))

let step2 w x y z data s =
  let w =
    Int32.add (Int32.add w data)
              (Int32.logxor y (Int32.logand z (Int32.logxor x y))) in
  Int32.add x
    (Int32.logor (Int32.shift_left w s) (Int32.shift_right_logical w (32-s)))

let step3 w x y z data s =
  let w =
    Int32.add (Int32.add w data)
              (Int32.logxor x (Int32.logxor y z)) in
  Int32.add x
    (Int32.logor (Int32.shift_left w s) (Int32.shift_right_logical w (32-s)))

let step4 w x y z data s =
  let w =
    Int32.add (Int32.add w data)
              (Int32.logxor y (Int32.logor x (Int32.logxor z (-1l)))) in
  Int32.add x
    (Int32.logor (Int32.shift_left w s) (Int32.shift_right_logical w (32-s)))

let transform ctx data =
    let a = ctx.a and b = ctx.b and c = ctx.c and d = ctx.d in

    let a = step1 a b c d (Int32.add data.(0) 0xd76aa478l) 7 in
    let d = step1 d a b c (Int32.add data.(1) 0xe8c7b756l) 12 in
    let c = step1 c d a b (Int32.add data.(2) 0x242070dbl) 17 in
    let b = step1 b c d a (Int32.add data.(3) 0xc1bdceeel) 22 in
    let a = step1 a b c d (Int32.add data.(4) 0xf57c0fafl) 7 in
    let d = step1 d a b c (Int32.add data.(5) 0x4787c62al) 12 in
    let c = step1 c d a b (Int32.add data.(6) 0xa8304613l) 17 in
    let b = step1 b c d a (Int32.add data.(7) 0xfd469501l) 22 in
    let a = step1 a b c d (Int32.add data.(8) 0x698098d8l) 7 in
    let d = step1 d a b c (Int32.add data.(9) 0x8b44f7afl) 12 in
    let c = step1 c d a b (Int32.add data.(10) 0xffff5bb1l) 17 in
    let b = step1 b c d a (Int32.add data.(11) 0x895cd7bel) 22 in
    let a = step1 a b c d (Int32.add data.(12) 0x6b901122l) 7 in
    let d = step1 d a b c (Int32.add data.(13) 0xfd987193l) 12 in
    let c = step1 c d a b (Int32.add data.(14) 0xa679438el) 17 in
    let b = step1 b c d a (Int32.add data.(15) 0x49b40821l) 22 in

    let a = step2 a b c d (Int32.add data.(1) 0xf61e2562l) 5 in
    let d = step2 d a b c (Int32.add data.(6) 0xc040b340l) 9 in
    let c = step2 c d a b (Int32.add data.(11) 0x265e5a51l) 14 in
    let b = step2 b c d a (Int32.add data.(0) 0xe9b6c7aal) 20 in
    let a = step2 a b c d (Int32.add data.(5) 0xd62f105dl) 5 in
    let d = step2 d a b c (Int32.add data.(10) 0x02441453l) 9 in
    let c = step2 c d a b (Int32.add data.(15) 0xd8a1e681l) 14 in
    let b = step2 b c d a (Int32.add data.(4) 0xe7d3fbc8l) 20 in
    let a = step2 a b c d (Int32.add data.(9) 0x21e1cde6l) 5 in
    let d = step2 d a b c (Int32.add data.(14) 0xc33707d6l) 9 in
    let c = step2 c d a b (Int32.add data.(3) 0xf4d50d87l) 14 in
    let b = step2 b c d a (Int32.add data.(8) 0x455a14edl) 20 in
    let a = step2 a b c d (Int32.add data.(13) 0xa9e3e905l) 5 in
    let d = step2 d a b c (Int32.add data.(2) 0xfcefa3f8l) 9 in
    let c = step2 c d a b (Int32.add data.(7) 0x676f02d9l) 14 in
    let b = step2 b c d a (Int32.add data.(12) 0x8d2a4c8al) 20 in

    let a = step3 a b c d (Int32.add data.(5) 0xfffa3942l) 4 in
    let d = step3 d a b c (Int32.add data.(8) 0x8771f681l) 11 in
    let c = step3 c d a b (Int32.add data.(11) 0x6d9d6122l) 16 in
    let b = step3 b c d a (Int32.add data.(14) 0xfde5380cl) 23 in
    let a = step3 a b c d (Int32.add data.(1) 0xa4beea44l) 4 in
    let d = step3 d a b c (Int32.add data.(4) 0x4bdecfa9l) 11 in
    let c = step3 c d a b (Int32.add data.(7) 0xf6bb4b60l) 16 in
    let b = step3 b c d a (Int32.add data.(10) 0xbebfbc70l) 23 in
    let a = step3 a b c d (Int32.add data.(13) 0x289b7ec6l) 4 in
    let d = step3 d a b c (Int32.add data.(0) 0xeaa127fal) 11 in
    let c = step3 c d a b (Int32.add data.(3) 0xd4ef3085l) 16 in
    let b = step3 b c d a (Int32.add data.(6) 0x04881d05l) 23 in
    let a = step3 a b c d (Int32.add data.(9) 0xd9d4d039l) 4 in
    let d = step3 d a b c (Int32.add data.(12) 0xe6db99e5l) 11 in
    let c = step3 c d a b (Int32.add data.(15) 0x1fa27cf8l) 16 in
    let b = step3 b c d a (Int32.add data.(2) 0xc4ac5665l) 23 in

    let a = step4 a b c d (Int32.add data.(0) 0xf4292244l) 6 in
    let d = step4 d a b c (Int32.add data.(7) 0x432aff97l) 10 in
    let c = step4 c d a b (Int32.add data.(14) 0xab9423a7l) 15 in
    let b = step4 b c d a (Int32.add data.(5) 0xfc93a039l) 21 in
    let a = step4 a b c d (Int32.add data.(12) 0x655b59c3l) 6 in
    let d = step4 d a b c (Int32.add data.(3) 0x8f0ccc92l) 10 in
    let c = step4 c d a b (Int32.add data.(10) 0xffeff47dl) 15 in
    let b = step4 b c d a (Int32.add data.(1) 0x85845dd1l) 21 in
    let a = step4 a b c d (Int32.add data.(8) 0x6fa87e4fl) 6 in
    let d = step4 d a b c (Int32.add data.(15) 0xfe2ce6e0l) 10 in
    let c = step4 c d a b (Int32.add data.(6) 0xa3014314l) 15 in
    let b = step4 b c d a (Int32.add data.(13) 0x4e0811a1l) 21 in
    let a = step4 a b c d (Int32.add data.(4) 0xf7537e82l) 6 in
    let d = step4 d a b c (Int32.add data.(11) 0xbd3af235l) 10 in
    let c = step4 c d a b (Int32.add data.(2) 0x2ad7d2bbl) 15 in
    let b = step4 b c d a (Int32.add data.(9) 0xeb86d391l) 21 in

    ctx.a <- Int32.add ctx.a a;
    ctx.b <- Int32.add ctx.b b;
    ctx.c <- Int32.add ctx.c c;
    ctx.d <- Int32.add ctx.d d

let string_to_data s =
  let data = Array.make 16 0l in
  for i = 0 to 15 do
    let j = i lsl 2 in
    data.(i) <-
      Int32.logor (Int32.shift_left (Int32.of_int (Char.code s.[j+3])) 24)
        (Int32.logor (Int32.shift_left (Int32.of_int (Char.code s.[j+2])) 16)
          (Int32.logor (Int32.shift_left (Int32.of_int (Char.code s.[j+1])) 8)
                       (Int32.of_int (Char.code s.[j]))))
  done;
  data

let int32_to_string n s i =
  s.[i+3] <- Char.chr (Int32.to_int (Int32.shift_right n 24) land 0xFF);
  s.[i+2] <- Char.chr (Int32.to_int (Int32.shift_right n 16) land 0xFF);
  s.[i+1] <- Char.chr (Int32.to_int (Int32.shift_right n 8) land 0xFF);
  s.[i] <- Char.chr (Int32.to_int n land 0xFF)

let init () =
  { buf = String.create 64;
    pos = 0;
    a = 0x67452301l;
    b = 0xefcdab89l;
    c = 0x98badcfel;
    d = 0x10325476l;
    bits = 0L }

let update ctx input ofs len =
  let rec upd ofs len =
    if len <= 0 then () else
    if ctx.pos + len < 64 then begin
      (* Just buffer the data *)
      String.blit input ofs ctx.buf ctx.pos len;
      ctx.pos <- ctx.pos + len
    end else begin
      (* Fill the buffer *)
      let len' = 64 - ctx.pos in
      if len' > 0 then String.blit input ofs ctx.buf ctx.pos len';
      (* Transform 64 bytes *)
      transform ctx (string_to_data ctx.buf);
      ctx.pos <- 0;
      upd (ofs + len') (len - len')
    end in
  upd ofs len;
  ctx.bits <- Int64.add ctx.bits (Int64.of_int (len lsl 3))


let finish ctx =
  let padding = String.make 64 '\000' in
  padding.[0] <- '\x80';
  let numbits = ctx.bits in
  if ctx.pos < 56 then begin
    update ctx padding 0 (56 - ctx.pos)
  end else begin
    update ctx padding 0 (64 + 56 - ctx.pos)
  end;
  assert (ctx.pos = 56);
  let data = string_to_data ctx.buf in
  data.(14) <- (Int64.to_int32 numbits);
  data.(15) <- (Int64.to_int32 (Int64.shift_right_logical numbits 32));
  transform ctx data;
  let res = String.create 16 in
  int32_to_string ctx.a res 0;
  int32_to_string ctx.b res 4;
  int32_to_string ctx.c res 8;
  int32_to_string ctx.d res 12;
  res

let test hex s =
  let ctx = init() in
  update ctx s 0 (String.length s);
  let res = finish ctx in
  let exp = Digest.string s in
  let ok = res = exp && Digest.to_hex exp = hex in
  if not ok then
    Printf.printf "Failure for %S : %S %S %S %S\n" s res exp
                  (Digest.to_hex exp) hex;
  ok

let time msg iter fn =
  let start = Sys.time() in
  for i = 1 to iter do fn () done;
  let stop = Sys.time() in
  printf "%s: %.2f s\n" msg (stop -. start)

let _ =
  (* Test *)
  if test "d41d8cd98f00b204e9800998ecf8427e" ""
  && test "0cc175b9c0f1b6a831c399e269772661" "a"
  && test "900150983cd24fb0d6963f7d28e17f72" "abc"
  && test "8215ef0796a20bcaaae116d3876c664a"
          "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"
  && test "7707d6ae4e027c70eea2a935c2296f21" (String.make 1_000_000 'a')
  && test "f96b697d7cb7938d525a2f31aaf161d0" "message digest"
  && test "d174ab98d277d9f5a5611c2c9f419d9f"
          "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
  && test "9e107d9d372bb6826bd81d3542a419d6"
          "The quick brown fox jumps over the lazy dog"
  && test "e4d909c290d0fb1ca068ffaddf22cbd0"
          "The quick brown fox jumps over the lazy dog."
  then printf "Test vectors passed.\n";
  flush stdout;
  (* Benchmark *)
  if (Array.length Sys.argv) > 1 && (Sys.argv.(1) = "-benchmark") then begin
    let s = String.make 50000 'a' in
    let num_iter = 1000 in
    time "OCaml implementation" num_iter
      (fun () ->
        let ctx = init() in
        update ctx s 0 (String.length s);
        ignore (finish ctx));
    time "C implementation" num_iter
      (fun () -> ignore (Digest.string s))
  end
