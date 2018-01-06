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

(* This module would  only work with js backend, since it requires
   [nativeint] behaves as js  numbers
 *)

(* TODO: see GPR#333
   the encoding of nativeint is platform dependent *)
open Nativeint

let (^) = Bs_string.append

let (>>>) = Nativeint.shift_right_logical
let (>>) = Nativeint.shift_right
let ( +~ ) = Nativeint.add
let ( *~ ) = Nativeint.mul
let ( & ) = Nativeint.logand
let ( << ) = Nativeint.shift_left
let lognot x = Nativeint.logxor x (-1n)

type t = {  hi : nativeint; lo : nativeint ;  }

let to_unsigned (x : nativeint) =
   x >>> 0

let mk ~lo ~hi = {lo = to_unsigned lo ; hi}

let min_int =  mk  ~lo: 0n ~hi:(-0x80000000n)

let max_int =
 mk  ~lo:( -0xffff_ffffn) ~hi: 0x7fff_ffffn

let one = mk ~lo: 1n ~hi:0n
let zero = mk ~lo: 0n ~hi: 0n
let neg_one = mk ~lo:(-1n) ~hi:(-1n)



let neg_signed x =  (x  & 0x8000_0000n) <> 0n

let add
    ({lo = this_low_; hi = this_high_} : t)
    ({lo = other_low_; hi = other_high_} : t) =
  let lo =  ( this_low_ +~ other_low_) &  0xffff_ffffn in
  let overflow =
    if (neg_signed this_low_ && (neg_signed other_low_  || not (neg_signed lo)))
       || (neg_signed other_low_  && not (neg_signed lo))
    then 1n
    else  0n
  in
  mk ~lo ~hi:(( this_high_ +~ other_high_ +~ overflow) &  0xffff_ffffn)


let not {lo; hi }  = mk ~lo:(lognot lo) ~hi:(lognot hi)

let eq x y = x.hi = y.hi && x.lo = y.lo

let equal_null x y =    
  match Js.nullToOption y with 
  | None -> false 
  | Some y -> eq x y 
let equal_undefined x y =   
  match Js.undefinedToOption y with 
  | None -> false 
  | Some y -> eq x y   
let equal_nullable x y =   
  match Js.toOption y with 
  | None -> false 
  | Some y -> eq x y 

let neg ({lo; hi} as x) =
  if eq x  min_int then
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
    mk ~lo:(Nativeint.shift_left lo numBits)
     ~hi:
       (Nativeint.logor
         ( lo >>>  (32 - numBits))
         (Nativeint.shift_left hi numBits))


let lsr_ ({lo; hi} as x) numBits =
  if numBits = 0 then x
  else
    let offset = numBits - 32 in
    if offset = 0 then
        mk ~lo:hi ~hi:0n
    else if offset > 0 then
      mk ~lo:(hi >>> offset) ~hi:0n
    else
      mk
      ~hi: ( hi >>> numBits)
        ~lo:(
          Nativeint.logor
            (Nativeint.shift_left hi (-offset))
            ( lo >>> numBits))


let asr_ ({lo; hi } as x) numBits =
  if numBits = 0  then
    x
  else
  if numBits < 32  then
    mk ~hi:(  hi >> numBits)
      ~lo:(
       Nativeint.logor
         ( hi << (32 - numBits)) (* zero filled *)
         ( lo >>> numBits))


  else
    mk ~hi:( if hi >= 0n then  0n  else -1n) ~lo:(  hi >> (numBits - 32))


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
        let c00 =  a00 *~ b00  in
        c16 :=  (c00 >>> 16) +~   a16 *~ b00 ;
        c32 :=  !c16 >>> 16;
        c16 :=  ( !c16 & 0xffffn) +~ a00 *~ b16;
        c32 :=  (!c32 +~  ( !c16 >>> 16)) +~  a32 *~ b00;
        c48 :=  !c32 >>>  16;
        c32 :=  (!c32 & 0xffffn) +~  a16 *~ b16;
        c48 :=  !c48 +~  ( !c32 >>> 16);
        c32 :=  (!c32 & 0xffffn) +~  a00 *~ b32;
        c48 :=  !c48 +~  (!c32 >>> 16);
        c32 :=  !c32 & 0xffffn;
        c48 :=  (!c48  +~ (a48 *~ b00 +~ a32 *~ b16 +~ a16 *~ b32 +~ a00 *~ b48)) & 0xffffn;
        mk ~lo:
           (Nativeint.logor
             (c00 & 0xffffn)
             ( (!c16 & 0xffffn) << 16))
         ~hi:( Nativeint.logor
             !c32
             ( !c48 << 16))

      end




let swap {lo ; hi } =
  mk ~lo:( Caml_int32.caml_int32_bswap hi)
    ~hi:( Caml_int32.caml_int32_bswap lo)

(* Dispatched by the compiler, idea: should we do maximum sharing
*)
 let xor {lo = this_lo; hi= this_hi} {lo = other_lo; hi = other_hi} =
   mk
     ~lo:(Nativeint.logxor this_lo other_lo)
    ~hi:(Nativeint.logxor this_hi other_hi)


let or_  {lo = this_lo; hi= this_hi} {lo = other_lo; hi = other_hi} =
  mk
    ~lo:(Nativeint.logor this_lo other_lo)
    ~hi:(Nativeint.logor this_hi other_hi)

let and_ {lo = this_lo; hi= this_hi} {lo = other_lo; hi = other_hi} =
  mk
    ~lo:(Nativeint.logand this_lo other_lo)
    ~hi:(Nativeint.logand this_hi other_hi)



(* TODO: if we encode lo int32 bit as unsigned then
   this is not necessary,
   however (x>>>0 >>>0) is not that bad
*)

type comparison = t -> t -> bool

let  ge ({hi; lo } : t)  ({hi = other_hi; lo = other_lo}) : bool =
  if hi > other_hi then true
  else if hi < other_hi then false
  else  lo  >=  other_lo



let neq x y = Pervasives.not (eq x y)
let lt x y  = Pervasives.not (ge x y)
let gt x y =
  if x.hi > y.hi then
    true
  else if x.hi < y.hi  then
    false
  else
     x.lo >  y.lo


let le x y = Pervasives.not (gt x y)
let min x y = if lt x  y then x else y 
let max x y = if gt x y then x else y 

let to_float ({hi; lo} : t) = 
  Nativeint.to_float ( hi *~ [%raw{|0x100000000|}] +~ lo)




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
  if Js_float.isNaN x
  ||  Pervasives.not  (Js_float.isFinite x ) then zero
  else if x <= neg_two_ptr_63 then
    min_int
  else if x  +. 1. >= two_ptr_63_dbl then
    max_int
  else if x < 0. then
    neg (of_float (-. x))
  else mk  ~lo:(Nativeint.of_float (mod_float  x two_ptr_32_dbl))
         ~hi:(Nativeint.of_float (x /. two_ptr_32_dbl))


external log2 : float = "Math.LN2" [@@bs.val]  
(* external maxFloat : float -> float -> float = "Math.max" [@@bs.val] *)

let rec div self other =
  match self, other with
  | _, {lo = 0n ; hi = 0n} ->
    raise Division_by_zero
  | {lo = 0n; hi = 0n}, _
    -> zero
  | {lo = 0n ; hi = -0x8000_0000n}, _
    ->
    begin
      if eq other one || eq other neg_one then self
      else if eq other min_int then one
      else
        let other_hi = other.hi in
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
  | {lo = _; hi = self_hi}, {lo = _; hi = other_hi}
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
        let approx = ref ( Pervasives.max 1.
             (floor (to_float !rem /. to_float other) )) in
        let log2 = ceil (log !approx /. log2) in
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


let div_mod self other =
  let quotient = div self other in
  quotient, sub self (mul quotient other)

let compare self other =
  let v = Pervasives.compare self.hi other.hi in
  if v = 0 then
    Pervasives.compare self.lo  other.lo
  else v

let of_int32 (lo : nativeint) =
  mk ~lo ~hi:(if lo < 0n then -1n else 0n)

let to_int32 x = Nativeint.logor x.lo  0n (* signed integer *)


(* width does matter, will it be relevant to endian order? *)

let to_hex x =
  let aux v =
    Bs_string.of_int (Nativeint.to_int @@ Nativeint.shift_right_logical v 0) ~base:16
  in
  match x.hi, x.lo with
  | 0n, 0n -> "0"
  | _, 0n -> aux x.hi ^ "00000000"
  | 0n, _ -> aux x.lo
  | _, _ ->
    let lo =  aux x.lo in
    let pad = 8 -Bs_string.length lo in
    if pad <= 0 then
      aux x.hi ^ lo
    else
      aux x.hi ^ Caml_utils.repeat pad "0" [@bs] ^ lo

let discard_sign x = {x with hi = Nativeint.logand 0x7fff_ffffn x.hi }


open Js_typed_array
let float_of_bits x  =
  let to_int32 (x : nativeint) = x |> Nativeint.to_int32
  in
  (*TODO:
    This should get inlined, we should apply a simple inliner in the js layer,
    the thing is its lambda representation is complex but after js layer,
    it's qutie simple
  *)
  let int32 = Int32_array.make  [| to_int32 x.lo; to_int32 x.hi |] in
   Float64_array.unsafe_get (Float64_array.fromBuffer (Int32_array.buffer int32)) 0

let  bits_of_float (x : float) =

  let to_nat (x : int32) = x |> Int32.to_int |>  Nativeint.of_int in

  let u = Float64_array.make [| x |] in
  let int32 = Int32_array.fromBuffer (Float64_array.buffer u) in
  mk ~lo:(to_nat (Int32_array.unsafe_get int32 0))
    ~hi:( to_nat (Int32_array.unsafe_get int32 1))

(** used by "%caml_string_get64" *)
let get64 (s : string) (i:int) : t =
  mk ~lo:
    (Nativeint.logor
       (Nativeint.logor
          (Nativeint.of_int (Char.code s.[i]))
          (Nativeint.of_int (Char.code s.[i+1]) << 8))
       (Nativeint.logor
          (Nativeint.of_int (Char.code s.[i+2]) << 16 )
          (Nativeint.of_int (Char.code s.[i+3]) << 24 )))
    ~hi:
      (Nativeint.logor
         (Nativeint.logor
            (Nativeint.of_int (Char.code s.[i+4]) << 32)
            (Nativeint.of_int (Char.code s.[i+5]) << 40))
         (Nativeint.logor
            (Nativeint.of_int (Char.code s.[i+6]) << 48 )
            (Nativeint.of_int (Char.code s.[i+7]) << 56 )))
