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
   [nativeint] behaves as js  numbers
 *)

(* TODO: see GPR#333
   the encoding of nativeint is platform dependent *)
   



let (>>>) = Caml_nativeint_extern.shift_right_logical
let (>>) = Caml_nativeint_extern.shift_right
let ( +~ ) = Caml_nativeint_extern.add
let ( *~ ) = Caml_nativeint_extern.mul
let ( & ) = Caml_nativeint_extern.logand
let ( << ) = Caml_nativeint_extern.shift_left
let lognot x = Caml_nativeint_extern.logxor x (-1n)

type t = Int64 of {  hi : nativeint; lo : nativeint ;  }

external unsafe_to_int64 : t -> int64 = "%identity"           
external unsafe_of_int64 : int64 -> t = "%identity"


let to_unsigned (x : nativeint) =
   x >>> 0

let mk ~lo ~hi = Int64 {lo = to_unsigned lo ; hi}
(*
module N = struct 
type nonrec t = t = private {  hi : nativeint; lo : nativeint ;  }
end 
open N
*)
let min_int =  mk  ~lo: 0n ~hi:(-0x80000000n)

let max_int =
 mk  ~lo:( -0xffff_ffffn) ~hi: 0x7fff_ffffn

let one = mk ~lo: 1n ~hi:0n
let zero = mk ~lo: 0n ~hi: 0n
let neg_one = mk ~lo:(-1n) ~hi:(-1n)



let neg_signed x =  (x  & 0x8000_0000n) <> 0n

let add
    (Int64 {lo = this_low_; hi = this_high_} : t)
    (Int64 {lo = other_low_; hi = other_high_} : t) =
  let lo =  ( this_low_ +~ other_low_) &  0xffff_ffffn in
  let overflow =
    if (neg_signed this_low_ && (neg_signed other_low_  || not (neg_signed lo)))
       || (neg_signed other_low_  && not (neg_signed lo))
    then 1n
    else  0n
  in
  mk ~lo ~hi:(( this_high_ +~ other_high_ +~ overflow) &  0xffff_ffffn)


let not (Int64 {lo; hi })  = mk ~lo:(lognot lo) ~hi:(lognot hi)

let eq (Int64 x) (Int64 y) = x.hi = y.hi && x.lo = y.lo

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

let neg x =
  if eq x  min_int then
    min_int
  else add (not x) one


let sub x y =
  add x (neg y)

let lsl_ (Int64 {lo; hi} as x) numBits =
  if numBits = 0 then
    x
  else if numBits >= 32 then
    mk ~lo:0n ~hi:(Caml_nativeint_extern.shift_left lo (numBits - 32))
  else
    mk ~lo:(Caml_nativeint_extern.shift_left lo numBits)
     ~hi:
       (Caml_nativeint_extern.logor
         ( lo >>>  (32 - numBits))
         (Caml_nativeint_extern.shift_left hi numBits))


let lsr_ (Int64 {lo; hi} as x) numBits =
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
          Caml_nativeint_extern.logor
            (Caml_nativeint_extern.shift_left hi (-offset))
            ( lo >>> numBits))


let asr_ (Int64 {lo; hi } as x) numBits =
  if numBits = 0  then
    x
  else
  if numBits < 32  then
    mk ~hi:(  hi >> numBits)
      ~lo:(
       Caml_nativeint_extern.logor
         ( hi << (32 - numBits)) (* zero filled *)
         ( lo >>> numBits))


  else
    mk ~hi:( if hi >= 0n then  0n  else -1n) ~lo:(  hi >> (numBits - 32))


let is_zero = function
  | Int64 {lo = 0n ; hi = 0n} -> true
  | _ -> false



let rec mul this
    other =
  match this, other with
  | Int64 {lo = 0n ; hi = 0n}, _
  | _, Int64 {lo = 0n; hi = 0n}
    -> zero
  | Int64 {lo = 0n; hi = - 0x80000000n}, Int64 {lo;_ }
  | Int64 {lo;_}, Int64 {lo = 0n; hi = - 0x80000000n}
    ->
    if  (lo & 0x1n) = 0n then
      zero
    else min_int
  | Int64 {lo = this_lo; hi = this_hi},
    Int64 {lo = other_lo; hi = other_hi }
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
        c16.contents <-  (c00 >>> 16) +~   a16 *~ b00 ;
        c32.contents <-  c16.contents >>> 16;
        c16.contents <-  ( c16.contents & 0xffffn) +~ a00 *~ b16;
        c32.contents <-  (c32.contents +~  ( c16.contents >>> 16)) +~  a32 *~ b00;
        c48.contents <-  c32.contents >>>  16;
        c32.contents <-  (c32.contents & 0xffffn) +~  a16 *~ b16;
        c48.contents <-  c48.contents +~  ( c32.contents >>> 16);
        c32.contents <-  (c32.contents & 0xffffn) +~  a00 *~ b32;
        c48.contents <-  c48.contents +~  (c32.contents >>> 16);
        c32.contents <-  c32.contents & 0xffffn;
        c48.contents <-  (c48.contents  +~ (a48 *~ b00 +~ a32 *~ b16 +~ a16 *~ b32 +~ a00 *~ b48)) & 0xffffn;
        mk ~lo:
           (Caml_nativeint_extern.logor
             (c00 & 0xffffn)
             ( (c16.contents & 0xffffn) << 16))
         ~hi:( Caml_nativeint_extern.logor
             c32.contents
             ( c48.contents << 16))

      end




let swap (Int64 {lo ; hi }) =
  mk ~lo:( Caml_int32.caml_int32_bswap hi)
    ~hi:( Caml_int32.caml_int32_bswap lo)

(* Dispatched by the compiler, idea: should we do maximum sharing
*)
 let xor (Int64 {lo = this_lo; hi= this_hi}) (Int64 {lo = other_lo; hi = other_hi}) =
   mk
     ~lo:(Caml_nativeint_extern.logxor this_lo other_lo)
    ~hi:(Caml_nativeint_extern.logxor this_hi other_hi)


let or_  (Int64 {lo = this_lo; hi= this_hi}) (Int64 {lo = other_lo; hi = other_hi}) =
  mk
    ~lo:(Caml_nativeint_extern.logor this_lo other_lo)
    ~hi:(Caml_nativeint_extern.logor this_hi other_hi)

let and_ (Int64 {lo = this_lo; hi= this_hi}) (Int64 {lo = other_lo; hi = other_hi}) =
  mk
    ~lo:(Caml_nativeint_extern.logand this_lo other_lo)
    ~hi:(Caml_nativeint_extern.logand this_hi other_hi)



(* TODO: if we encode lo int32 bit as unsigned then
   this is not necessary,
   however (x>>>0 >>>0) is not that bad
*)

type comparison = t -> t -> bool

let  ge (Int64 {hi; lo } : t)  (Int64 {hi = other_hi; lo = other_lo}) : bool =
  if hi > other_hi then true
  else if hi < other_hi then false
  else  lo  >=  other_lo



let neq x y = Pervasives.not (eq x y)
let lt x y  = Pervasives.not (ge x y)
let gt (Int64 x) (Int64 y) =
  if x.hi > y.hi then
    true
  else if x.hi < y.hi  then
    false
  else
     x.lo >  y.lo


let le x y = Pervasives.not (gt x y)
let min x y = if lt x  y then x else y 
let max x y = if gt x y then x else y 

let to_float (Int64 {hi; lo} : t) = 
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



let rec to_string (Int64{hi=self_hi; lo= self_lo} as self : t) = 
  if self_hi = 0n then Caml_nativeint_extern.to_string self_lo
  else 
  if self_hi <0n then 
    if eq self min_int then "-9223372036854775808"
    else "-" ^ to_string (neg self)
  else (* large positive number *)
  let approx_div1 =  (of_float (floor (to_float self /. 10.) )) in
  let (Int64 { lo = rem_lo ;hi = rem_hi} as rem) = 
    sub (sub self (mul approx_div1 (mk ~lo:9n ~hi:0n))) approx_div1 in 
  if rem_lo =0n && rem_hi = 0n then to_string approx_div1 ^ "0"
  else 
  if rem_hi < 0n then 
    let (Int64 {lo = rem_lo}) = neg rem in     
    let rem_lo = Caml_nativeint_extern.to_float rem_lo in 
    let delta =  (ceil (rem_lo /. 10.)) in 
    let remainder = 10. *. delta -. rem_lo in
    to_string (add approx_div1 
      (neg (mk ~lo:(Caml_nativeint_extern.of_float delta) ~hi:0n))) ^ 
        Caml_nativeint_extern.to_string (Caml_nativeint_extern.of_float remainder)
  else 
    let rem_lo = Caml_nativeint_extern.to_float rem_lo in 
    let delta =  (floor (rem_lo /. 10.)) in 
    let remainder = rem_lo -. 10. *. delta in 
    to_string (add approx_div1 (mk ~lo:(Caml_nativeint_extern.of_float delta) ~hi:0n)) ^
     Caml_nativeint_extern.to_string (Caml_nativeint_extern.of_float remainder) 
    
    

let rec div self other =
  match self, other with
  | _, Int64 {lo = 0n ; hi = 0n} ->
    raise Division_by_zero
  | Int64 {lo = 0n; hi = 0n}, _
    -> zero
  | Int64 {lo = 0n ; hi = -0x8000_0000n}, _
    ->
    begin
      if eq other one || eq other neg_one then self
      else if eq other min_int then one
      else
        let (Int64 {hi = other_hi;_}) = other in
      (* now |other| >= 2, so |this/other| < |MIN_VALUE|*)
        let half_this = asr_ self 1  in
        let approx = lsl_ (div half_this other) 1 in
        match approx with
        | Int64 {lo = 0n ; hi = 0n}
          -> if other_hi < 0n then one else neg one
        | _
          ->
          let rem = sub self (mul other approx) in
          add approx (div rem other)
    end
  | _, Int64 {lo = 0n; hi = - 0x8000_0000n}
    -> zero
  | Int64 {lo = _; hi = self_hi}, Int64 {lo = _; hi = other_hi}
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
      while ge rem.contents other  do
        let approx = ref ( Pervasives.max 1.
             (Caml_float.floor (to_float rem.contents /. to_float other) )) in
        let log2 = ceil (log approx.contents /. log2) in
        let delta =
          if log2 <= 48. then 1.
          else 2. ** (log2 -. 48.) in
        let approxRes = ref (of_float approx.contents) in
        let approxRem = ref (mul approxRes.contents other) in
        while (match approxRem.contents with Int64 {hi;_}-> hi) < 0n || gt approxRem.contents rem.contents do
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

let compare (Int64 self) (Int64 other) =
  let v = Pervasives.compare self.hi other.hi in
  if v = 0 then
    Pervasives.compare self.lo  other.lo
  else v

let of_int32 (lo : nativeint) =
  mk ~lo ~hi:(if lo < 0n then -1n else 0n)

let to_int32 (Int64 x) = Caml_nativeint_extern.logor x.lo  0n (* signed integer *)


(* width does matter, will it be relevant to endian order? *)

let to_hex (x : int64) =
  let Int64 {hi = x_hi; lo = x_lo} = unsafe_of_int64 x in 
  let aux v : string =
    Caml_string_extern.of_int (Caml_nativeint_extern.to_int (Caml_nativeint_extern.shift_right_logical v 0)) ~base:16
  in
  match x_hi, x_lo with
  | 0n, 0n -> "0"
  | _, 0n -> aux x_hi ^ "00000000"
  | 0n, _ -> aux x_lo
  | _, _ ->
    let lo =  aux x_lo in
    let pad = 8 -Caml_string_extern.length lo in
    if pad <= 0 then
      aux x_hi ^ lo
    else
      aux x_hi ^ Caml_utils.repeat pad "0"  ^ lo


let discard_sign (x : int64) : int64 = 
  let v = unsafe_of_int64 x in   
  unsafe_to_int64 
  (match v with Int64 v -> Int64 {  v with hi = Caml_nativeint_extern.logand 0x7fff_ffffn v.hi })

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

let float_of_bits (Int64 x : t) : float =  
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
    let buf = ([%raw{|function(x){return new Int32Array(new Float64Array([x]).buffer)}|}] : _ -> _) x in 
    mk ~lo:(fst buf) ~hi:(snd buf)
  (* let to_nat (x : int32) = x |> Caml_int32_extern.to_int |>  Caml_nativeint_extern.of_int in

  let u = Float64_array.make [| x |] in
  let int32 = Int32_array.fromBuffer (Float64_array.buffer u) in
  mk ~lo:(to_nat (Int32_array.unsafe_get int32 0))
    ~hi:( to_nat (Int32_array.unsafe_get int32 1)) *)

(** used by "%caml_string_get64" *)
(* let get64 (s : string) (i:int) : t =
  let module String = Caml_string_extern in 
  mk ~lo:
    (Caml_nativeint_extern.logor
       (Caml_nativeint_extern.logor
          (Caml_nativeint_extern.of_int (Caml_char.code s.[i]))
          (Caml_nativeint_extern.of_int (Caml_char.code s.[i+1]) << 8))
       (Caml_nativeint_extern.logor
          (Caml_nativeint_extern.of_int (Caml_char.code s.[i+2]) << 16 )
          (Caml_nativeint_extern.of_int (Caml_char.code s.[i+3]) << 24 )))
    ~hi:
      (Caml_nativeint_extern.logor
         (Caml_nativeint_extern.logor
            (Caml_nativeint_extern.of_int (Caml_char.code s.[i+4]) << 32)
            (Caml_nativeint_extern.of_int (Caml_char.code s.[i+5]) << 40))
         (Caml_nativeint_extern.logor
            (Caml_nativeint_extern.of_int (Caml_char.code s.[i+6]) << 48 )
            (Caml_nativeint_extern.of_int (Caml_char.code s.[i+7]) << 56 )))
 *)

