(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)





(** *)

let (+~) = Int32.add 
let add32 = Int32.add
let (<<) = Int32.shift_left 
let (>>>) = Int32.shift_right_logical
let (>>) = Int32.shift_right
let (&) = Int32.logand  
let (^) = Int32.logxor 
let lognot n = Int32.logxor n (-1l)
let cmn q a b x s t = 
    let a = a +~ q +~ x +~ t in
    Int32.logor (a << s)  (a >>> (32 - s)) +~  b


let  f a b c d x s t = 
  cmn (Int32.logor (b & c)  (lognot b & d)) a b x s t


let g a b c d x s t =
  cmn (Int32.logor (b & d)  (c & (lognot d))) a b x s t

;;
let h a b c d x s t = 
  cmn (b ^ c ^ d) a b x s t
;;

let i a b c d x s t = 
  cmn (c ^ (Int32.logor b  (lognot d))) a b x s t

let cycle (x : int32 array)  (k : int32 array) = 
    let a = ref x.(0) in 
    let b = ref x.(1) in 
    let c = ref x.(2) in 
    let d = ref x.(3) in

    a := f !a !b !c !d k.(0) 7 0xd76aa478l;
    d := f !d !a !b !c k.(1) 12 0xe8c7b756l;
    c := f !c !d !a !b k.(2) 17  0x242070dbl;
    b := f !b !c !d !a k.(3) 22 0xc1bdceeel;

    a := f !a !b !c !d k.(4) 7  0xf57c0fafl;
    d := f !d  !a  !b  !c  k.(5)  12   0x4787c62al;
    c := f !c  !d  !a  !b  k.(6)  17  0xa8304613l;
    b := f !b  !c  !d  !a  k.(7)  22  0xfd469501l;

    a := f !a  !b  !c  !d  k.(8)  7   0x698098d8l;
    d := f !d  !a  !b  !c  k.(9)  12  0x8b44f7afl;
    c := f !c  !d  !a  !b  k.(10)  17  0xffff5bb1l;
    b := f !b  !c  !d  !a  k.(11)  22  0x895cd7bel;
    a := f !a  !b  !c  !d  k.(12)  7   0x6b901122l;
    d := f !d  !a  !b  !c  k.(13)  12  0xfd987193l;
    c := f !c  !d  !a  !b  k.(14)  17  0xa679438el;
    b := f !b  !c  !d  !a  k.(15)  22   0x49b40821l;

    a := g !a  !b  !c  !d  k.(1)  5  0xf61e2562l;
    d := g !d  !a  !b  !c  k.(6)  9  0xc040b340l;
    c := g !c  !d  !a  !b  k.(11)  14   0x265e5a51l;
    b := g !b  !c  !d  !a  k.(0)  20  0xe9b6c7aal;
    a := g !a  !b  !c  !d  k.(5)  5  0xd62f105dl;
    d := g !d  !a  !b  !c  k.(10)  9   0x2441453l;
    c := g !c  !d  !a  !b  k.(15)  14  0xd8a1e681l;
    b := g !b  !c  !d  !a  k.(4)  20  0xe7d3fbc8l;
    a := g !a  !b  !c  !d  k.(9)  5   0x21e1cde6l;
    d := g !d  !a  !b  !c  k.(14)  9  0xc33707d6l;
    c := g !c  !d  !a  !b  k.(3)  14  0xf4d50d87l;
    b := g !b  !c  !d  !a  k.(8)  20   0x455a14edl;
    a := g !a  !b  !c  !d  k.(13)  5  0xa9e3e905l;
    d := g !d  !a  !b  !c  k.(2)  9  0xfcefa3f8l;
    c := g !c  !d  !a  !b  k.(7)  14   0x676f02d9l;
    b := g !b  !c  !d  !a  k.(12)  20  0x8d2a4c8al;

    a := h !a  !b  !c  !d  k.(5)  4  0xfffa3942l;
    d := h !d  !a  !b  !c  k.(8)  11  0x8771f681l;
    c := h !c  !d  !a  !b  k.(11)  16   0x6d9d6122l;
    b := h !b  !c  !d  !a  k.(14)  23  0xfde5380cl;
    a := h !a  !b  !c  !d  k.(1)  4  0xa4beea44l;
    d := h !d  !a  !b  !c  k.(4)  11   0x4bdecfa9l;
    c := h !c  !d  !a  !b  k.(7)  16  0xf6bb4b60l;
    b := h !b  !c  !d  !a  k.(10)  23  0xbebfbc70l;
    a := h !a  !b  !c  !d  k.(13)  4   0x289b7ec6l;
    d := h !d  !a  !b  !c  k.(0)  11  0xeaa127fal;
    c := h !c  !d  !a  !b  k.(3)  16  0xd4ef3085l;
    b := h !b  !c  !d  !a  k.(6)  23   0x4881d05l;
    a := h !a  !b  !c  !d  k.(9)  4  0xd9d4d039l;
    d := h !d  !a  !b  !c  k.(12)  11  0xe6db99e5l;
    c := h !c  !d  !a  !b  k.(15)  16   0x1fa27cf8l;
    b := h !b  !c  !d  !a  k.(2)  23  0xc4ac5665l;

    a := i !a  !b  !c  !d  k.(0)  6  0xf4292244l;
    d := i !d  !a  !b  !c  k.(7)  10   0x432aff97l;
    c := i !c  !d  !a  !b  k.(14)  15  0xab9423a7l;
    b := i !b  !c  !d  !a  k.(5)  21  0xfc93a039l;
    a := i !a  !b  !c  !d  k.(12)  6   0x655b59c3l;
    d := i !d  !a  !b  !c  k.(3)  10  0x8f0ccc92l;
    c := i !c  !d  !a  !b  k.(10)  15  0xffeff47dl;
    b := i !b  !c  !d  !a  k.(1)  21  0x85845dd1l;
    a := i !a  !b  !c  !d  k.(8)  6   0x6fa87e4fl;
    d := i !d  !a  !b  !c  k.(15)  10  0xfe2ce6e0l;
    c := i !c  !d  !a  !b  k.(6)  15  0xa3014314l;
    b := i !b  !c  !d  !a  k.(13)  21   0x4e0811a1l;
    a := i !a  !b  !c  !d  k.(4)  6  0xf7537e82l;
    d := i !d  !a  !b  !c  k.(11)  10  0xbd3af235l;
    c := i !c  !d  !a  !b  k.(2)  15   0x2ad7d2bbl;
    b := i !b  !c  !d  !a  k.(9)  21  0xeb86d391l;

    x.(0) <- !a +~ x.(0);
    x.(1) <- !b +~ x.(1);
    x.(2) <- !c +~ x.(2);
    x.(3) <- !d +~ x.(3)


let seed_a = 0x67452301l
let seed_b = 0xefcdab89l
let seed_c = 0x98badcfel
let seed_d = 0x10325476l 

let state = [| seed_a; seed_b ; seed_c; seed_d |]

let md5blk = [|
    0l;0l;0l;0l;
    0l;0l;0l;0l;
    0l;0l;0l;0l; 
    0l;0l;0l;0l
  |] 

let caml_md5_string s start len = 
  let s = Bs_string.slice   s start len in
  let n =Bs_string.length s in
  let () = 
    state.(0) <- seed_a; 
    state.(1) <- seed_b; 
    state.(2) <- seed_c;
    state.(3) <- seed_d ;
    for i = 0 to 15 do 
      md5blk.(i) <- 0l
    done
  in  


  let i_end = n / 64 in
  for  i = 1 to  i_end do 
    for j = 0 to 16 - 1 do 
      let k = i * 64 - 64 + j * 4 in 
      md5blk.(j) <- (Int32.of_int (Char.code s.[k])) +~
                    (Int32.of_int (Char.code s.[k+1]) << 8 ) +~        
                    (Int32.of_int (Char.code s.[k+2]) << 16 ) +~        
                    (Int32.of_int (Char.code s.[k+3]) << 24 )
    done ;
    cycle state md5blk
  done ;

  let s_tail  = Bs_string.slice_rest s (i_end  * 64) in 
  for kk = 0 to 15 do 
    md5blk.(kk) <- 0l 
  done ;
  let i_end =Bs_string.length s_tail - 1 in
  for i = 0 to  i_end do 
    md5blk.(i / 4 ) <- 
      Int32.logor md5blk.(i / 4)  (Int32.of_int (Char.code s_tail.[i]) << ((i mod 4) lsl 3))
  done ;
  let i = i_end + 1 in
  md5blk.(i / 4 ) <-  Int32.logor md5blk.(i / 4 )  (0x80l << ((i mod 4) lsl 3)) ;
  if i > 55 then
    begin 
      cycle state md5blk;
      for i = 0 to 15 do 
        md5blk.(i) <- 0l
      done 
    end;
  md5blk.(14) <-  Int32.mul (Int32.of_int n)  8l;
  cycle state md5blk;
  Bs_string.of_small_int32_array [|
        state.(0) & 0xffl;
        (state.(0) >> 8) & 0xffl;
        (state.(0) >> 16) & 0xffl;
        (state.(0) >> 24) & 0xffl;

        state.(1) & 0xffl;
        (state.(1) >> 8) & 0xffl;
        (state.(1) >> 16) & 0xffl;
        (state.(1) >> 24) & 0xffl;

        state.(2) & 0xffl;
        (state.(2) >> 8) & 0xffl;
        (state.(2) >> 16) & 0xffl;
        (state.(2) >> 24) & 0xffl;

        state.(3) & 0xffl;
        (state.(3) >> 8) & 0xffl;
        (state.(3) >> 16) & 0xffl;
        (state.(3) >> 24) & 0xffl;

  |]






