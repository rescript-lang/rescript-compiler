(* BuckleScript compiler
 * Copyright (C) 2015-2016 Bloomberg Finance L.P.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

(* Author: Hongbo Zhang *)


let idiv (x:nativeint) (y:nativeint) = 
  if y = 0n  then
    raise Division_by_zero
  else Nativeint.div x y

let imod (x : nativeint) (y:nativeint) = 
  if y = 0n then
    raise Division_by_zero
  else Nativeint.rem x  y


let caml_bswap16 (x : nativeint) = 
 let open Nativeint in 
 logor (shift_left (logand x 0x00ffn) 8)
    (shift_right_logical (logand x 0xff00n) 8)

let caml_int32_bswap (x : nativeint) = 
  let open Nativeint in 
  logor (shift_left (logand x  0x000000FFn) 24)
     (logor (shift_left (logand x  0x0000FF00n)  8)
        (logor (shift_right_logical (logand x  0x00FF0000n)  8) 
      (shift_right_logical (logand x  0xFF000000n)  24)))



let caml_nativeint_bswap = caml_int32_bswap

let imul : int32 -> int32 -> int32 = [%bs.raw{| Math.imul || function (x,y) {
  y |= 0; return ((((x >> 16) * y) << 16) + (x & 0xffff) * y)|0; 
}
|}]
