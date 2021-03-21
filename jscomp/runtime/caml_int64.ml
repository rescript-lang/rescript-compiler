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



(* This module would  only work with js backend, since it requires
   [int] behaves as js  numbers
*)

(* TODO: see GPR#333
   the encoding of int is platform dependent *)




let %private 
{
  shift_right_logical =  (>>>~);
  add = (+~);
  mul = ( *~ )
} = 
  (module Caml_nativeint_extern)

let {i64_eq = eq ;
  i64_ge = ge;
  i64_gt = gt;
  } = (module Caml) 

let lognot x =  x lxor (-1)

(* [hi] is signed 
   [lo] is unsigned

   signedness does not matter when they are doing int32 bits operation   
   however, they are different when doing comparison
*)
type t =   Caml_int64_extern.t = {
  hi : int ; [@as "0"] lo : int ; [@as "1" ] 
}

external unsafe_to_int64 : t -> int64 = "%identity"           
external unsafe_of_int64 : int64 -> t = "%identity"


let [@inline] mk ~lo ~hi =  {lo = lo >>>~ 0 ; hi}
let min_int =  mk  ~lo: 0 ~hi:(0x80000000)
(* The high bits are signed 0x80000000 |~ 0 *)

let max_int =
  mk  ~lo:(0xffff_ffff) ~hi: 0x7fff_ffff

let one = mk ~lo: 1 ~hi:0
let zero = mk ~lo: 0 ~hi: 0
let neg_one = mk ~lo:(-1) ~hi:(-1)



let neg_signed x =  (x  land 0x8000_0000) <> 0
let non_neg_signed x = (x  land 0x8000_0000) = 0
let succ_aux ~x_lo ~x_hi = 
  let lo =  ( x_lo +~ 1) lor 0 in  
  mk ~lo ~hi:(( x_hi +~ if lo = 0 then 1 else 0) lor 0)
let succ ( {lo = x_lo; hi = x_hi} : t) =
  succ_aux ~x_lo ~x_hi

let neg ( {lo;hi} ) =
  let other_lo = (lognot lo +~  1) lor 0 in   
  mk ~lo:other_lo 
    ~hi:((lognot hi +~ if other_lo = 0 then 1 else 0)  lor 0)






let add_aux 
    ( {lo = x_lo; hi = x_hi} : t)
    ~y_lo ~y_hi  =
  let lo =  ( x_lo +~ y_lo) lor 0 in
  let overflow =
    if (neg_signed x_lo && ( neg_signed y_lo  ||  (non_neg_signed lo)))
    || (neg_signed y_lo  &&  (non_neg_signed lo))
       (* we can make it symmetric by adding (neg_signed x_lo) but it will make it 
          verbose and slow
          a (b+c) + b (a+c)
          --> bc + ac + ab 
          --> a (b+c) + bc
       *)
    then 1 
    else  0
  in
  mk ~lo ~hi:(( x_hi +~ y_hi +~ overflow) lor 0)


let add
    (self : t)
    ( {lo = y_lo; hi = y_hi} : t) =
  add_aux self ~y_lo ~y_hi


(* let not ( {lo; hi })  = mk ~lo:(lognot lo) ~hi:(lognot hi) *)



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



(* when [lo] is unsigned integer, [lognot lo] is still an unsigned integer  *)
let sub_aux x ~lo ~hi = 
  let y_lo =  (lognot lo +~  1) >>>~ 0 in 
  let y_hi =  ((lognot hi +~ if y_lo = 0 then 1 else 0)  lor  0) in 
  add_aux x ~y_lo ~y_hi

let sub self ({lo;hi})= sub_aux self ~lo ~hi 


let lsl_ ( {lo; hi} as x) numBits =
  if numBits = 0 then
    x
  else if numBits >= 32 then
    mk ~lo:0 ~hi:(lo lsl (numBits - 32))
  else
    mk ~lo:(lo lsl numBits)
      ~hi:
        (
          ( lo >>>~  (32 - numBits)) lor
          ( hi lsl numBits))


let lsr_ ( {lo; hi} as x) numBits =
  if numBits = 0 then x
  else
    let offset = numBits - 32 in
    if offset = 0 then
      mk ~lo:hi ~hi:0
    else if offset > 0 then
      mk ~lo:(hi >>>~ offset) ~hi:0
    else
      mk
        ~hi: ( hi >>>~ numBits)
        ~lo:(          
          (hi lsl (-offset))
          lor
          ( lo >>>~ numBits))


let asr_ ( {lo; hi } as x) numBits =
  if numBits = 0  then
    x
  else
  if numBits < 32  then
    mk ~hi:(  hi asr numBits)
      ~lo:(
        ( hi lsl (32 - numBits)) (* zero filled *)
        lor
        ( lo >>>~ numBits))


  else
    mk ~hi:( if hi >= 0 then  0  else -1) ~lo:(  hi asr (numBits - 32))


let is_zero = function
  |  {lo = 0 ; hi = 0} -> true
  | _ -> false



let rec mul this
    other =
  match this, other with
  |  {lo = 0 ; hi = 0}, _
  | _,  {lo = 0; hi = 0}
    -> zero
  |  {lo = 0; hi = - 0x80000000},  {lo;_ }
  |  {lo;_},  {lo = 0; hi = - 0x80000000}
    ->
    if lo land 0x1 = 0 then
      zero
    else min_int
  |  {lo = this_lo; hi = this_hi},
     {lo = other_lo; hi = other_hi }
    ->
    if this_hi < 0  then
      if other_hi < 0 then
        mul (neg this) (neg other)
      else
        neg (mul (neg this) other)
    else if other_hi < 0 then
      neg (mul this (neg other) )
    else
      (* TODO: when both are small, use float multiplication *)
      let a48 = this_hi >>>~ 16 in
      let a32 =  this_hi land 0xffff in
      let a16 =  this_lo >>>~ 16 in
      let a00 =  this_lo land 0xffff in

      let b48 =  other_hi >>>~ 16 in
      let b32 =  other_hi land 0xffff in
      let b16 =  other_lo >>>~ 16 in
      let b00 =  other_lo land 0xffff in

      let c48 = ref 0 in
      let c32 = ref 0 in
      let c16 = ref 0 in
      begin
        let c00 =  a00 *~ b00  in
        c16.contents <-  (c00 >>>~ 16) +~   a16 *~ b00 ;
        c32.contents <-  c16.contents >>>~ 16;
        c16.contents <-  ( c16.contents land 0xffff) +~ a00 *~ b16;
        c32.contents <-  (c32.contents +~  ( c16.contents >>>~ 16)) +~  a32 *~ b00;
        c48.contents <-  c32.contents >>>~  16;
        c32.contents <-  (c32.contents land 0xffff) +~  a16 *~ b16;
        c48.contents <-  c48.contents +~  ( c32.contents >>>~ 16);
        c32.contents <-  (c32.contents land 0xffff) +~  a00 *~ b32;
        c48.contents <-  c48.contents +~  (c32.contents >>>~ 16);
        c32.contents <-  c32.contents land 0xffff;
        c48.contents <-  (c48.contents  +~ (a48 *~ b00 +~ a32 *~ b16 +~ a16 *~ b32 +~ a00 *~ b48)) land 0xffff;
        mk ~lo:
          (
            (c00 land 0xffff) lor
            ( (c16.contents land 0xffff) lsl 16))
          ~hi:( 
            c32.contents lor
            ( c48.contents lsl 16))

      end





(* Dispatched by the compiler, idea: should we do maximum sharing
*)
let xor ( {lo = this_lo; hi= this_hi}) ( {lo = other_lo; hi = other_hi}) =
  mk
    ~lo:(this_lo lxor other_lo)
    ~hi:(this_hi lxor other_hi)


let or_  ( {lo = this_lo; hi= this_hi}) ( {lo = other_lo; hi = other_hi}) =
  mk
    ~lo:(this_lo lor other_lo )
    ~hi:(this_hi lor other_hi)

let and_ ( {lo = this_lo; hi= this_hi}) ( {lo = other_lo; hi = other_hi}) =
  mk
    ~lo:(this_lo land other_lo)
    ~hi:(this_hi land other_hi)



(* TODO: if we encode lo int32 bit as unsigned then
   this is not necessary,
   however (x>>>0 >>>0) is not that bad
*)







let to_float ( {hi; lo} : t) = 
  Caml_nativeint_extern.to_float ( hi *~ [%raw{|0x100000000|}] +~ lo)




(** sign: Positive  
    -FIXME: hex notation
*)
let two_ptr_32_dbl = 4294967296. (* 2. ** 32*)
let two_ptr_63_dbl = 9.22337203685477581e+18 (* 2. ** 63.*)
let neg_two_ptr_63 = -9.22337203685477581e+18 (*-. (2. ** 63.)*)

external mod_float : float -> float -> float = "caml_fmod_float"
(* note that we make sure the const number can acutally be represented
   {[
     (2. ** 63. -. 1. = 2. ** 63.) ;;
   ]}
*)


let rec of_float (x : float) : t =
  if Caml_float_extern.isNaN x
  ||  Pervasives.not  (Caml_float_extern.isFinite x ) then zero
  else if x <= neg_two_ptr_63 then
    min_int
  else if x  +. 1. >= two_ptr_63_dbl then
    max_int (* Undefined behavior *)
  else if x < 0. then
    neg (of_float (-. x))
  else mk  ~lo:(Caml_nativeint_extern.of_float (mod_float  x two_ptr_32_dbl))
      ~hi:(Caml_nativeint_extern.of_float (x /. two_ptr_32_dbl))


external log2 : float = "LN2" [@@bs.val]  [@@bs.scope "Math"]
external log : float -> float =  "log" [@@bs.val] [@@bs.scope "Math"]
external ceil : float -> float =  "ceil" [@@bs.val] [@@bs.scope "Math"]
external floor : float -> float =  "floor" [@@bs.val] [@@bs.scope "Math"]
(* external maxFloat : float -> float -> float = "Math.max" [@@bs.val] *)

(* either top 11 bits are all 0 or all 1 
   when it is all 1, we need exclude -2^53
*)
let isSafeInteger ({hi;lo}) = 
  let top11Bits = hi asr 21 in   
  top11Bits = 0 || 
  (top11Bits = -1 && 
   Pervasives.not (lo = 0 && hi = 0xff_e0_00_00))

external string_of_float : float -> string = "String" [@@bs.val] 
let rec to_string ( self : int64) = 
  let ({hi=self_hi;_} as self) = unsafe_of_int64 self in
  if isSafeInteger self then 
    string_of_float (to_float self)
  else 

  if self_hi <0 then 
    if eq self min_int then "-9223372036854775808"
    else "-" ^ to_string (unsafe_to_int64 (neg self))
  else (* large positive number *)    
    let ( {lo ; hi} as approx_div1) =  (of_float (floor (to_float self /. 10.) )) in
    let ( { lo = rem_lo ;hi = rem_hi} ) = (* rem should be a pretty small number *)
      self 
      |. sub_aux  ~lo:(lo lsl 3) ~hi:((lo>>>~29) lor (hi lsl 3))
      |. sub_aux ~lo:(lo lsl 1) ~hi: ((lo >>>~ 31) lor (hi lsl 1))
    in 
    if rem_lo =0 && rem_hi = 0 then to_string (unsafe_to_int64 approx_div1) ^ "0"
    else 
    if rem_hi < 0 then 
      (* let ( {lo = rem_lo}) = neg rem in      *)
      let rem_lo = (lognot rem_lo +~ 1 ) >>>~ 0 |. Caml_nativeint_extern.to_float  in 
      let delta =  (ceil (rem_lo /. 10.)) in 
      let remainder = 10. *. delta -. rem_lo in
      (
        approx_div1 
        |. sub_aux ~lo:(Caml_nativeint_extern.of_float delta) ~hi:0
        |. unsafe_to_int64 
        |. to_string
      ) ^ 
      Caml_nativeint_extern.to_string (Caml_nativeint_extern.of_float remainder)
    else 
      let rem_lo = Caml_nativeint_extern.to_float rem_lo in 
      let delta =  (floor (rem_lo /. 10.)) in 
      let remainder = rem_lo -. 10. *. delta in 
      (approx_div1 
       |. add_aux ~y_lo:(Caml_nativeint_extern.of_float delta) ~y_hi:0
       |. unsafe_to_int64 
       |. to_string)
      ^                                                    
      Caml_nativeint_extern.to_string (Caml_nativeint_extern.of_float remainder) 


let [@inline] float_max (a : float) b = 
  if a > b then a else b 
let rec div self other =
  match self, other with
  | _,  {lo = 0 ; hi = 0} ->
    raise Division_by_zero
  |  {lo = 0; hi = 0}, _
    -> zero
  |  {lo = 0 ; hi = -0x8000_0000}, _
    ->
    begin
      if eq other one || eq other neg_one then self
      else if eq other min_int then one
      else
        let ( {hi = other_hi;_}) = other in
        (* now |other| >= 2, so |this/other| < |MIN_VALUE|*)
        let half_this = asr_ self 1  in
        let approx = lsl_ (div half_this other) 1 in
        match approx with
        |  {lo = 0 ; hi = 0}
          -> if other_hi < 0 then one else neg one
        | _
          ->
          let rem = sub self (mul other approx) in
          add approx (div rem other)
    end
  | _,  {lo = 0; hi = - 0x8000_0000}
    -> zero
  |  {lo = _; hi = self_hi},  {lo = _; hi = other_hi}
    ->
    if self_hi < 0 then
      if other_hi <0 then
        div (neg self) (neg other)
      else
        neg (div (neg self)  other)
    else if other_hi < 0  then
      neg (div self (neg other))
    else
      let res = ref zero in
      let rem = ref self in
      (* assert false *)
      while ge rem.contents other  do
        let approx = ref ( float_max 1.
                             (Caml_float.floor (to_float rem.contents /. to_float other) )) in
        let log2 = ceil (log approx.contents /. log2) in
        let delta =
          if log2 <= 48. then 1.
          else 2. ** (log2 -. 48.) in
        let approxRes = ref (of_float approx.contents) in
        let approxRem = ref (mul approxRes.contents other) in
        while (match approxRem.contents with  {hi;_}-> hi) < 0 || gt approxRem.contents rem.contents do
          approx.contents <- approx.contents -. delta;
          approxRes.contents <- of_float approx.contents;
          approxRem.contents <- mul approxRes.contents other
        done;
        (if is_zero approxRes.contents then
           approxRes.contents <- one);
        res.contents <- add res.contents approxRes.contents;
        rem.contents <- sub rem.contents approxRem.contents
      done;
      res.contents

let mod_ self other =
  sub self (mul (div self other) other)


let div_mod (self : int64) (other : int64) : int64 * int64 =
  let quotient = div (unsafe_of_int64 self) (unsafe_of_int64 other) in
  unsafe_to_int64 quotient, unsafe_to_int64 (sub (unsafe_of_int64 self) (mul quotient (unsafe_of_int64 other)))

(** Note this function is unasfe here, but when combined it is actually safe
    In theory, we need do an uint_compare for [lo] components
    The thing is [uint_compare] and [int_compare] are specialised 
    to the same code when translted into js
*)
let [@inline] int_compare (x : int)  y = 
  if x < y then -1 else if x = y then 0 else 1 

let compare ( self) ( other) =
  let v = int_compare self.hi other.hi in
  if v = 0 then
    int_compare self.lo  other.lo
  else v

let of_int32 (lo : int) =
  mk ~lo ~hi:(if lo < 0 then -1 else 0)

let to_int32 ( x) = x.lo lor 0 (* signed integer *)


(* width does matter, will it be relevant to endian order? *)

let to_hex (x : int64) =
  let  {hi = x_hi; lo = x_lo} = unsafe_of_int64 x in 
  let aux v : string =
    Caml_string_extern.of_int (Caml_nativeint_extern.shift_right_logical v 0) ~base:16
  in
  match x_hi, x_lo with
  | 0, 0 -> "0"
  | _, 0 -> aux x_hi ^ "00000000"
  | 0, _ -> aux x_lo
  | _, _ ->
    let lo =  aux x_lo in
    let pad = 8 -Caml_string_extern.length lo in
    if pad <= 0 then
      aux x_hi ^ lo
    else
      aux x_hi ^ Caml_string_extern.repeat "0" pad   ^ lo


let discard_sign (x : int64) : int64 = 
  let v = unsafe_of_int64 x in   
  unsafe_to_int64 
    (match v with  v ->  {  v with hi = 0x7fff_ffff land v.hi })

(* >>> 0 does not change its bit representation
      it simply makes sure it is an unsigned integer
      -1 >>> 0 -> 4294967295
      Which is still (-1) if you interpret it as a signed integer
      When we do the call (new Int32Array(x[1], x[0]), it will
      convert x[0] from an unsigned integer to signed integer
   {[
     new Int32Array([-1 >>> 0])
       Int32Array(1)[-1]
   ]}
*)

let float_of_bits ( x : t) : float =  
  ([%raw{|function(lo,hi){ return (new Float64Array(new Int32Array([lo,hi]).buffer))[0]}|}] : _ -> _ -> _ ) x.lo x.hi 

(* let to_int32 (x : nativeint) = x |> Caml_nativeint_extern.to_int32
   in
   (*TODO:
   This should get inlined, we should apply a simple inliner in the js layer,
   the thing is its lambda representation is complex but after js layer,
   it's qutie simple
 *)
   let int32 = Int32_array.make  [| to_int32 x.lo; to_int32 x.hi |] in
   Float64_array.unsafe_get (Float64_array.fromBuffer (Int32_array.buffer int32)) 0 *)

let  bits_of_float : float -> t  = fun x -> 
  let lo,hi = ([%raw{|function(x){return new Int32Array(new Float64Array([x]).buffer)}|}] : _ -> _) x in 
  mk ~lo ~hi
