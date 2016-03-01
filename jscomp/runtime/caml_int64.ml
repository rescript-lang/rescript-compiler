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

(* Author: Hongbo Zhang  *)


(* This module would  only work with js backend, since it requires 
   [nativeint] behaves as js  numbers 
 *)

open Nativeint
type t = { lo : nativeint ; hi : nativeint}


let min_int = { lo = 0n; hi =  0x80000000n }

let one = {lo = 1n; hi = 0n}
let zero = {lo = 0n; hi = 0n}

let not {lo; hi }  = {lo = lognot lo; hi = lognot hi } 

let of_int32 (lo : nativeint) = 
  if lo < 0n then 
    {lo ; hi = -1n }
  else {lo ; hi = 0n }


let add 
    ({lo = this_low_; hi = this_high_} : t ) 
    ({lo = other_low_; hi = other_high_} : t ) = 

  let a48 = shift_right_logical this_high_  16 in
  let a32 = logand this_high_  0xFFFFn in
  let a16 = shift_right_logical this_low_  16 in
  let a00 = logand this_low_  0xFFFFn in

  let b48 = shift_right_logical other_high_  16 in
  let b32 = logand other_high_   0xFFFFn in
  let b16 = shift_right_logical other_low_  16 in
  let b00 = logand other_low_  0xFFFFn in

  begin 
    let c00 = add a00  b00 in
    let c16 = add (add (shift_right_logical c00  16) a16)  b16 in
    let c32 = add (add (shift_right_logical  c16  16) a32)  b32 in
    let c48 = add (add (shift_right_logical c32 16) a48)  b48 in
    {lo = 
       logor (shift_left (logand c16  0xFFFFn)  16)  (logand c00  0xFFFFn); 
     hi =  logor (shift_left (logand c48 0xFFFFn)  16)  (logand c32  0xFFFFn)
    }
  end 
let neg ({lo; hi} as x) =
  if x == min_int then 
    min_int 
  else add (not x) one


let sub x y = 
  add x (neg y)

let lsl_ ({lo; hi} as x) numBits = 
  if numBits = 0 then
    x 
  else if numBits >= 32 then 
    {lo =0n; hi = Nativeint.shift_left lo (numBits - 32) }
  else 
    {lo = Nativeint.shift_left lo numBits; 
     hi = 
       Nativeint.logor 
         (Nativeint.shift_right_logical lo (32 - numBits))
         (Nativeint.shift_left hi numBits)
    }

let lsr_ ({lo; hi} as x) numBits =   
  if numBits = 0 then x 
  else 
    let offset = numBits - 32 in
    if offset = 0 then  
      {hi = 0n; lo = hi}
    else if offset > 0 then 
      {hi = 0n; lo = Nativeint.shift_right_logical hi offset  }
    else 
      { hi = Nativeint.shift_right_logical hi numBits;
        lo = 
          Nativeint.logor 
            (Nativeint.shift_left hi (-offset))
            (Nativeint.shift_right_logical lo numBits)
      }      

let asr_ ({lo; hi } as x) numBits = 
  if numBits = 0  then
    x 
  else
  if numBits < 32  then
    {
      hi = Nativeint.shift_right hi numBits;
      lo = 
       Nativeint.logor
         (Nativeint.shift_left hi (32 - numBits)) (* zero filled *)
         (Nativeint.shift_right_logical lo numBits);

    }    
  else 
    {
      hi = if hi >= 0n then  0n  else -1n;
      lo = Nativeint.shift_right hi (numBits - 32)
    }
  
let is_zero = function
  | {lo = 0n ; hi = 0n} -> true
  | _ -> false

let min_int = {lo = 0n; hi = 0x80000000n}

let rec mul this 
    other = 
  match this, other with 
  | {lo = 0n ; hi = 0n}, _ 
  | _, {lo = 0n; hi = 0n}
    -> zero
  | {lo = 0n; hi = 0x80000000n}, {lo }
  | {lo}, {lo = 0n; hi = 0x80000000n}
    ->
    if Nativeint.logand lo 0x1n = 0n then
      zero
    else min_int 
  | {lo = this_lo; hi = this_hi}, 
    {lo = other_lo; hi = other_hi }
    -> 
    if this_hi < 0n  then 
      if other_hi < 0n then
        mul (neg this) (neg other)
      else
        neg (mul (neg this) other)
    else if other_hi < 0n then 
      neg (mul this (neg other) )
    else 
      (* TODO: when both are small, use float multiplication *)
      let a48 = Nativeint.shift_right_logical this_hi 16 in       
      let a32 = Nativeint.logand this_hi 0xffffn in
      let a16 = Nativeint.shift_right_logical this_lo 16 in 
      let a00 = Nativeint.logand this_lo 0xffffn in

      let b48 = Nativeint.shift_right_logical other_hi 16 in
      let b32 = Nativeint.logand other_hi 0xffffn in
      let b16 = Nativeint.shift_right_logical other_hi 16 in
      let b00 = Nativeint.logand other_hi 0xffffn in

      let c48 = ref 0n in
      let c32 = ref 0n in
      let c16 = ref 0n in 
      let c00 = ref 0n in
      begin
        c00 := Nativeint.mul a00 b00  ;
        c16 := Nativeint.shift_right_logical !c00 16;
        c00 := Nativeint.logand !c00 0xffffn;

        c16 := Nativeint.add !c16 (Nativeint.mul a16 b00 );
        c32 := Nativeint.shift_right_logical !c16 16;
        c16 := Nativeint.logand !c16 0xffffn;

        c16 := Nativeint.add !c16 (Nativeint.mul a00 b16);
        c32 := Nativeint.add !c32 (Nativeint.shift_right_logical !c16 16);
        c16 := Nativeint.logand !c16 0xffffn;
        
        c32 := Nativeint.add !c32 (Nativeint.mul a32 b00);
        c48 := Nativeint.shift_right_logical !c32 16;
        c32 := Nativeint.logand !c32 0xffffn;

        c32 := Nativeint.add !c32 (Nativeint.mul a16 b16);
        c48 := Nativeint.add !c48 (Nativeint.shift_right_logical !c32 16);
        c32 := Nativeint.logand !c32 0xffffn;

        c32 := Nativeint.add !c32 (Nativeint.mul a00 b32);
        c48 := Nativeint.add !c48 ( Nativeint.shift_right_logical !c32 16);
        c32 := Nativeint.logand !c32 0xffffn;

        c48 := 
          (* TODO: investigate why [@@] not optimized at best *)
          Nativeint.add !c48 (
          Nativeint.add (Nativeint.mul a48 b00) (
          Nativeint.add (Nativeint.mul a32 b16) (
          Nativeint.add (Nativeint.mul a16 b32) (
          (Nativeint.mul a00 b48)))));
        c48 := Nativeint.logand !c48 0xffffn;
        {lo = 
           Nativeint.logor 
             !c00
             (Nativeint.shift_left !c16 16);
         hi = Nativeint.logor
             !c32
             (Nativeint.shift_left !c48 16)
        }
      end

(* Dispatched by the compiler, idea: should we do maximum sharing 
*)
(* let xor {lo = this_lo; hi= this_hi} {lo = other_lo; hi = other_hi} =  *)
(*   { *)
(*     lo = Nativeint.logxor this_lo other_lo;  *)
(*    hi = Nativeint.logxor this_hi other_hi  *)
(*   } *)

