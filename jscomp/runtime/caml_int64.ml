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

let (>>>) = Nativeint.shift_right_logical
let (>>) = Nativeint.shift_right
let (+) = Nativeint.add 
let ( * ) = Nativeint.mul 
let ( & ) = Nativeint.logand
let ( << ) = Nativeint.shift_left
let lognot x = Nativeint.logxor x (-1n)

type t = { lo : nativeint ; hi : nativeint}


let min_int = { lo = 0n; hi =  -0x80000000n }

let max_int = { lo = -0xffff_ffffn; hi = 0x7fff_fffn }

let one = {lo = 1n; hi = 0n}
let zero = {lo = 0n; hi = 0n}

let of_int32 (lo : nativeint) = 
  if lo < 0n then 
    {lo ; hi = -1n }
  else {lo ; hi = 0n }

let add
    ({lo = this_low_; hi = this_high_} : t)
    ({lo = other_low_; hi = other_high_} : t) =
  let low = logand (add this_low_ other_low_) 0xffff_ffffn in
  let overflow = if this_low_ < 0n
    then other_low_ < 0n || low >= 0n
    else other_low_ < 0n && low >= 0n
  in
  let high = if overflow then
    logand (add 1n (add this_high_ other_high_)) 0xffff_ffffn
  else
    logand (add this_high_ other_high_) 0xffff_ffffn
  in
  { lo = low; hi = high }

let not {lo; hi }  = {lo = lognot lo; hi = lognot hi }

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
         ( lo >>>  (32 - numBits))
         (Nativeint.shift_left hi numBits)
    }

let lsr_ ({lo; hi} as x) numBits =   
  if numBits = 0 then x 
  else 
    let offset = numBits - 32 in
    if offset = 0 then  
      {hi = 0n; lo = hi}
    else if offset > 0 then 
      {hi = 0n; lo =  hi >>> offset  }
    else 
      { hi =  hi >>> numBits;
        lo = 
          Nativeint.logor 
            (Nativeint.shift_left hi (-offset))
            ( lo >>> numBits)
      }      

let asr_ ({lo; hi } as x) numBits = 
  if numBits = 0  then
    x 
  else
  if numBits < 32  then
    {
      hi =  hi >> numBits;
      lo = 
       Nativeint.logor
         ( hi << (32 - numBits)) (* zero filled *)
         ( lo >>> numBits);

    }    
  else 
    {
      hi = if hi >= 0n then  0n  else -1n;
      lo =  hi >> (numBits - 32)
    }
  
let is_zero = function
  | {lo = 0n ; hi = 0n} -> true
  | _ -> false



let rec mul this 
    other = 
  match this, other with 
  | {lo = 0n ; hi = 0n}, _ 
  | _, {lo = 0n; hi = 0n}
    -> zero
  | {lo = 0n; hi = - 0x80000000n}, {lo }
  | {lo}, {lo = 0n; hi = - 0x80000000n}
    ->
    if  (lo & 0x1n) = 0n then
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
      let a48 = this_hi >>> 16 in       
      let a32 =  this_hi & 0xffffn in
      let a16 =  this_lo >>> 16 in 
      let a00 =  this_lo & 0xffffn in

      let b48 =  other_hi >>> 16 in
      let b32 =  other_hi & 0xffffn in
      let b16 =  other_lo >>> 16 in
      let b00 =  other_lo & 0xffffn in

      let c48 = ref 0n in
      let c32 = ref 0n in
      let c16 = ref 0n in 
      begin
        let c00 =  a00 * b00  in
        c16 :=  (c00 >>> 16) +   a16 * b00 ;
        c32 :=  !c16 >>> 16;
        c16 :=  ( !c16 & 0xffffn) + a00 * b16;
        c32 :=  (!c32 +  ( !c16 >>> 16)) +  a32 * b00;
        c48 :=  !c32 >>>  16;
        c32 :=  (!c32 & 0xffffn) +  a16 * b16;
        c48 :=  !c48 +  ( !c32 >>> 16);
        c32 :=  (!c32 & 0xffffn) +  a00 * b32;
        c48 :=  !c48 +  (!c32 >>> 16);
        c32 :=  !c32 & 0xffffn;
        c48 :=  (!c48  + (a48 * b00 + a32 * b16 + 
           a16 * b32 + a00 * b48)) & 0xffffn;
        {lo = 
           Nativeint.logor 
             (c00 & 0xffffn)
             ( (!c16 & 0xffffn) << 16);
         hi = Nativeint.logor
             !c32
             ( !c48 << 16)
        }
      end


external bswap32: nativeint -> nativeint = "%bswap_int32"

let swap {lo ; hi } = 
  {lo = bswap32 hi; hi = bswap32 lo }

(* Dispatched by the compiler, idea: should we do maximum sharing 
*)
(* let xor {lo = this_lo; hi= this_hi} {lo = other_lo; hi = other_hi} =  *)
(*   { *)
(*     lo = Nativeint.logxor this_lo other_lo;  *)
(*    hi = Nativeint.logxor this_hi other_hi  *)
(*   } *)

(* TODO: if we encode lo int32 bit as unsigned then 
   this is not necessary, 
   however (x>>>0 >>>0) is not that bad
*)
let to_unsigned (x : nativeint) = 
   x >>> 0

type comparison = t -> t -> bool 

let  ge ({hi; lo } : t)  ({hi = other_hi; lo = other_lo}) : bool = 
  if hi > other_hi then true
  else if hi < other_hi then false 
  else (to_unsigned lo ) >= (to_unsigned other_lo)

let eq x y = x.hi = y.hi && x.lo = y.lo

let neq x y = Pervasives.not (eq x y)
let lt x y  = Pervasives.not (ge x y)
let gt x y = 
  if x.hi > y.hi then
    true
  else if x.hi < y.hi  then
    false
  else 
    to_unsigned x.lo > to_unsigned y.lo

  
let le x y = Pervasives.not (gt x y)


let to_float ({lo; hi } : t) : float = 
  (* The low 32-bits as an unsigned value *)
  let low_bits_unsigned =
    if lo >= 0n   then
      lo
    else  lo +  0x1_0000_0000n in
  Nativeint.to_float ( hi *   0x1_0000_0000n +  low_bits_unsigned)






(** sign: Positive  *)
let two_ptr_32_dbl = 2. ** 32.
let two_ptr_63_dbl = 2. ** 63.
let neg_two_ptr_63 = -. (2. ** 63.)


(* note that we make sure the const number can acutally be represented 
   {[ 
     (2. ** 63. -. 1. = 2. ** 63.) ;;
   ]}
*)
(* let max_int_as_dbl = Int64.to_float 0x7fff_ffff_ffff_ffffL *)
(* let min_int_as_dbl = Int64.to_float 0x8000_0000_0000_0000L 
   TODO: (E.math   ) constant folding
*)
 
(* Note in ocaml [Int64.of_float] is weird
   {[
     Int64.of_float 2.e65;;
     - : int64 = -9223372036854775808L
   ]}
   {[
     Int64.of_float (Int64.to_float (Int64.sub Int64.max_int 1L));;
     - : int64 = -9223372036854775808L
   ]}
*)
let rec of_float (x : float) : t = 
  if Js.Float.is_nan x ||  Pervasives.not  (Js.Float.is_finite x ) then zero 
  else if x <= neg_two_ptr_63 then 
    min_int
  else if x  +. 1. >= two_ptr_63_dbl then
    max_int 
  else if x < 0. then 
    neg (of_float (-. x))
  else { lo = Nativeint.of_float (mod_float  x two_ptr_32_dbl) ;  
         hi =  Nativeint.of_float (x /. two_ptr_32_dbl)  }



let rec div self other = 
  match self, other with
  | _, {lo = 0n ; hi = 0n} -> 
    raise Division_by_zero
  | {lo = 0n; hi = 0n}, _ 
    -> zero 
  | {lo = 0n ; hi = -0x8000_0000n}, _
    -> 
    begin match other with 
    | ({ lo = 1n ; hi = 0n}
      |{ lo = -1n; hi = -1n}) 
      -> self (* -MIN_VALUE = MIN_VALUE*)
    | {lo = 0n ; hi = -0x8000_0000n} 
      -> one
    | {hi = other_hi ; _} -> 
      (* now |other| >= 2, so |this/other| < |MIN_VALUE|*)
      let half_this = asr_ self 1  in        
      let approx = lsl_ (div half_this other) 1 in
      match approx with 
      | {lo = 0n ; hi = 0n}
        -> if other_hi < 0n then one else neg one
      | _ 
        -> 
        let rem = sub self (mul other approx) in
        add approx (div rem other)
    end
  | _, {lo = 0n; hi = - 0x8000_0000n} 
    -> zero
  | {lo = self_lo; hi = self_hi}, {lo = other_lo; hi = other_hi} 
    -> 
    if self_hi < 0n then 
      if other_hi <0n then 
        div (neg self) (neg other)
      else
        neg (div (neg self)  other)
    else if other_hi < 0n  then 
      neg (div self (neg other))
    else 
      let res = ref zero in
      let rem = ref self in 
      (* assert false *)
      while ge !rem other  do
        let approx = ref ( Js.Float.max 1.
             (floor (to_float !rem /. to_float other) )) in
        let log2 = ceil (log !approx /. Js.Float.log2) in
        let delta =
          if log2 <= 48. then 1.
          else 2. ** (log2 -. 48.) in
        let approxRes = ref (of_float !approx) in 
        let approxRem = ref (mul !approxRes other) in 
        while !approxRem.hi < 0n || gt !approxRem !rem do
          approx := !approx -. delta;
          approxRes := of_float !approx;
          approxRem := mul !approxRes other
        done;
        (if is_zero !approxRes then
          approxRes := one);
        res := add !res !approxRes;
        rem := sub !rem !approxRem
      done;
      !res 

let mod_ self other = 
  sub self (mul (div self other) other)

let compare self other = 
  let v = Pervasives.compare self.hi other.hi in
  if v = 0 then 
    Pervasives.compare 
      (to_unsigned self.lo) (to_unsigned other.lo)
  else v 

(* let rec to_string self : string =  *)
(*   match self with *)
(*   | {lo=0n; hi = 0n}  *)
(*     ->  *)
(*     "0" *)
(*   | {lo = 0n ; hi = - 0x8000_0000n} *)
(*     ->  *)
(*     "-9223372036854775808" (\* Int64.min_int *\) *)
(*   | {lo; hi} ->  *)
(*     if  hi < 0n then "-" ^ to_string (neg self) *)
(*     else  *)

(*       let radixToPower = 1000000L *)
