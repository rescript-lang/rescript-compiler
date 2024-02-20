/* Copyright (C) 2015-2016 Bloomberg Finance L.P.
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
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. */

/* This module would  only work with js backend, since it requires
   [int] behaves as js  numbers
*/

/* TODO: see GPR#333
 the encoding of int is platform dependent */

%%private(
  let {shift_right_logical: \">>>~", add: \"+~", mul: \"*~"} = module(Caml_nativeint_extern)
)

let {i64_eq: eq, i64_ge: ge, i64_gt: gt} = module(Caml)

let lognot = x => lxor(x, -1)

/* [hi] is signed 
   [lo] is unsigned

   signedness does not matter when they are doing int32 bits operation   
   however, they are different when doing comparison
*/
type t = Caml_int64_extern.t = {@as("0") hi: int, @as("1") lo: int}

external unsafe_to_int64: t => int64 = "%identity"
external unsafe_of_int64: int64 => t = "%identity"

@inline let mk = (~lo, ~hi) => {lo: \">>>~"(lo, 0), hi}
let min_int = mk(~lo=0, ~hi=0x80000000)
/* The high bits are signed 0x80000000 |~ 0 */

let max_int = mk(~lo=0xffff_ffff, ~hi=0x7fff_ffff)

let one = mk(~lo=1, ~hi=0)
let zero = mk(~lo=0, ~hi=0)
let neg_one = mk(~lo=-1, ~hi=-1)

let neg_signed = x => land(x, 0x8000_0000) != 0
let non_neg_signed = x => land(x, 0x8000_0000) == 0
let succ_aux = (~x_lo, ~x_hi) => {
  let lo = lor(\"+~"(x_lo, 1), 0)
  mk(
    ~lo,
    ~hi=lor(
      \"+~"(
        x_hi,
        if lo == 0 {
          1
        } else {
          0
        },
      ),
      0,
    ),
  )
}
let succ = ({lo: x_lo, hi: x_hi}: t) => succ_aux(~x_lo, ~x_hi)

let neg = ({lo, hi}) => {
  let other_lo = lor(\"+~"(lognot(lo), 1), 0)
  mk(
    ~lo=other_lo,
    ~hi=lor(
      \"+~"(
        lognot(hi),
        if other_lo == 0 {
          1
        } else {
          0
        },
      ),
      0,
    ),
  )
}

let add_aux = ({lo: x_lo, hi: x_hi}: t, ~y_lo, ~y_hi) => {
  let lo = lor(\"+~"(x_lo, y_lo), 0)
  let overflow = if (
    (neg_signed(x_lo) && (neg_signed(y_lo) || non_neg_signed(lo))) ||
      (neg_signed(y_lo) && non_neg_signed(lo))
  ) {
    /* we can make it symmetric by adding (neg_signed x_lo) but it will make it 
          verbose and slow
          a (b+c) + b (a+c)
          --> bc + ac + ab 
          --> a (b+c) + bc
 */
    1
  } else {
    0
  }

  mk(~lo, ~hi=lor(\"+~"(\"+~"(x_hi, y_hi), overflow), 0))
}

let add = (self: t, {lo: y_lo, hi: y_hi}: t) => add_aux(self, ~y_lo, ~y_hi)

/* let not ( {lo; hi })  = mk ~lo:(lognot lo) ~hi:(lognot hi) */

let equal = (x, y) => x.lo == y.lo && x.hi == y.hi
let equal_null = (x, y) =>
  switch Js.nullToOption(y) {
  | None => false
  | Some(y) => eq(x, y)
  }
let equal_undefined = (x, y) =>
  switch Js.undefinedToOption(y) {
  | None => false
  | Some(y) => eq(x, y)
  }
let equal_nullable = (x, y) =>
  switch Js.toOption(y) {
  | None => false
  | Some(y) => eq(x, y)
  }

/* when [lo] is unsigned integer, [lognot lo] is still an unsigned integer */
let sub_aux = (x, ~lo, ~hi) => {
  let y_lo = \">>>~"(\"+~"(lognot(lo), 1), 0)
  let y_hi = lor(
    \"+~"(
      lognot(hi),
      if y_lo == 0 {
        1
      } else {
        0
      },
    ),
    0,
  )
  add_aux(x, ~y_lo, ~y_hi)
}

let sub = (self, {lo, hi}) => sub_aux(self, ~lo, ~hi)

let lsl_ = ({lo, hi} as x, numBits) =>
  if numBits == 0 {
    x
  } else if numBits >= 32 {
    mk(~lo=0, ~hi=lsl(lo, numBits - 32))
  } else {
    mk(~lo=lsl(lo, numBits), ~hi=lor(\">>>~"(lo, 32 - numBits), lsl(hi, numBits)))
  }

let lsr_ = ({lo, hi} as x, numBits) =>
  if numBits == 0 {
    x
  } else {
    let offset = numBits - 32
    if offset == 0 {
      mk(~lo=hi, ~hi=0)
    } else if offset > 0 {
      mk(~lo=\">>>~"(hi, offset), ~hi=0)
    } else {
      mk(~hi=\">>>~"(hi, numBits), ~lo=lor(lsl(hi, -offset), \">>>~"(lo, numBits)))
    }
  }

let asr_ = ({lo, hi} as x, numBits) =>
  if numBits == 0 {
    x
  } else if numBits < 32 {
    mk(
      ~hi=asr(hi, numBits),
      ~lo=/* zero filled */
      lor(lsl(hi, 32 - numBits), \">>>~"(lo, numBits)),
    )
  } else {
    mk(
      ~hi=if hi >= 0 {
        0
      } else {
        -1
      },
      ~lo=asr(hi, numBits - 32),
    )
  }

let is_zero = x =>
  switch x {
  | {lo: 0, hi: 0} => true
  | _ => false
  }

let rec mul = (this, other) =>
  switch (this, other) {
  | ({lo: 0, hi: 0}, _)
  | (_, {lo: 0, hi: 0}) => zero
  | ({lo: 0, hi: -0x80000000}, {lo, _})
  | ({lo, _}, {lo: 0, hi: -0x80000000}) =>
    if land(lo, 0x1) == 0 {
      zero
    } else {
      min_int
    }
  | ({lo: this_lo, hi: this_hi}, {lo: other_lo, hi: other_hi}) =>
    if this_hi < 0 {
      if other_hi < 0 {
        mul(neg(this), neg(other))
      } else {
        neg(mul(neg(this), other))
      }
    } else if other_hi < 0 {
      neg(mul(this, neg(other)))
    } else {
      /* TODO: when both are small, use float multiplication */
      let a48 = \">>>~"(this_hi, 16)
      let a32 = land(this_hi, 0xffff)
      let a16 = \">>>~"(this_lo, 16)
      let a00 = land(this_lo, 0xffff)

      let b48 = \">>>~"(other_hi, 16)
      let b32 = land(other_hi, 0xffff)
      let b16 = \">>>~"(other_lo, 16)
      let b00 = land(other_lo, 0xffff)

      let c48 = ref(0)
      let c32 = ref(0)
      let c16 = ref(0)

      let c00 = \"*~"(a00, b00)
      c16.contents = \"+~"(\">>>~"(c00, 16), \"*~"(a16, b00))
      c32.contents = \">>>~"(c16.contents, 16)
      c16.contents = \"+~"(land(c16.contents, 0xffff), \"*~"(a00, b16))
      c32.contents = \"+~"(\"+~"(c32.contents, \">>>~"(c16.contents, 16)), \"*~"(a32, b00))
      c48.contents = \">>>~"(c32.contents, 16)
      c32.contents = \"+~"(land(c32.contents, 0xffff), \"*~"(a16, b16))
      c48.contents = \"+~"(c48.contents, \">>>~"(c32.contents, 16))
      c32.contents = \"+~"(land(c32.contents, 0xffff), \"*~"(a00, b32))
      c48.contents = \"+~"(c48.contents, \">>>~"(c32.contents, 16))
      c32.contents = land(c32.contents, 0xffff)
      c48.contents = land(
        \"+~"(
          c48.contents,
          \"+~"(\"+~"(\"+~"(\"*~"(a48, b00), \"*~"(a32, b16)), \"*~"(a16, b32)), \"*~"(a00, b48)),
        ),
        0xffff,
      )
      mk(
        ~lo=lor(land(c00, 0xffff), lsl(land(c16.contents, 0xffff), 16)),
        ~hi=lor(c32.contents, lsl(c48.contents, 16)),
      )
    }
  }

/* Dispatched by the compiler, idea: should we do maximum sharing
 */
let xor = ({lo: this_lo, hi: this_hi}, {lo: other_lo, hi: other_hi}) =>
  mk(~lo=lxor(this_lo, other_lo), ~hi=lxor(this_hi, other_hi))

let or_ = ({lo: this_lo, hi: this_hi}, {lo: other_lo, hi: other_hi}) =>
  mk(~lo=lor(this_lo, other_lo), ~hi=lor(this_hi, other_hi))

let and_ = ({lo: this_lo, hi: this_hi}, {lo: other_lo, hi: other_hi}) =>
  mk(~lo=land(this_lo, other_lo), ~hi=land(this_hi, other_hi))

/* TODO: if we encode lo int32 bit as unsigned then
   this is not necessary,
   however (x>>>0 >>>0) is not that bad
*/

let to_float = ({hi, lo}: t) =>
  Caml_nativeint_extern.to_float(\"+~"(\"*~"(hi, %raw(`0x100000000`)), lo))

/** sign: Positive  
    -FIXME: hex notation
*/
let two_ptr_32_dbl = 4294967296. /* 2. ** 32 */
let two_ptr_63_dbl = 9.22337203685477581e+18 /* 2. ** 63. */
let neg_two_ptr_63 = -9.22337203685477581e+18 /* -. (2. ** 63.) */

external mod_float: (float, float) => float = "?fmod_float"
/* note that we make sure the const number can acutally be represented
   {[
     (2. ** 63. -. 1. = 2. ** 63.) ;;
   ]}
*/

let rec of_float = (x: float): t =>
  if Caml_float_extern.isNaN(x) || Pervasives.not(Caml_float_extern.isFinite(x)) {
    zero
  } else if x <= neg_two_ptr_63 {
    min_int
  } else if x +. 1. >= two_ptr_63_dbl {
    max_int /* Undefined behavior */
  } else if x < 0. {
    neg(of_float(-.x))
  } else {
    mk(
      ~lo=Caml_nativeint_extern.of_float(mod_float(x, two_ptr_32_dbl)),
      ~hi=Caml_nativeint_extern.of_float(x /. two_ptr_32_dbl),
    )
  }

@val @scope("Math") external log2: float = "LN2"
@val @scope("Math") external log: float => float = "log"
@val @scope("Math") external ceil: float => float = "ceil"
@val @scope("Math") external floor: float => float = "floor"
/* external maxFloat : float -> float -> float = "Math.max" [@@val] */

/* either top 11 bits are all 0 or all 1 
   when it is all 1, we need exclude -2^53
*/
let isSafeInteger = ({hi, lo}) => {
  let top11Bits = asr(hi, 21)
  top11Bits == 0 || (top11Bits == -1 && Pervasives.not(lo == 0 && hi == 0xff_e0_00_00))
}

@val external string_of_float: float => string = "String"
let rec to_string = (self: int64) => {
  let {hi: self_hi, _} as self = unsafe_of_int64(self)
  if isSafeInteger(self) {
    string_of_float(to_float(self))
  } else if self_hi < 0 {
    if eq(self, min_int) {
      "-9223372036854775808"
    } else {
      "-" ++ to_string(unsafe_to_int64(neg(self)))
    }
  } else {
    /* large positive number */
    let {lo, hi} as approx_div1 = of_float(floor(to_float(self) /. 10.))
    let {lo: rem_lo, hi: rem_hi} =
      /* rem should be a pretty small number */
      self
      ->sub_aux(~lo=lsl(lo, 3), ~hi=lor(\">>>~"(lo, 29), lsl(hi, 3)))
      ->sub_aux(~lo=lsl(lo, 1), ~hi=lor(\">>>~"(lo, 31), lsl(hi, 1)))

    if rem_lo == 0 && rem_hi == 0 {
      to_string(unsafe_to_int64(approx_div1)) ++ "0"
    } else if rem_hi < 0 {
      /* let ( {lo = rem_lo}) = neg rem in */
      let rem_lo = \">>>~"(\"+~"(lognot(rem_lo), 1), 0)->Caml_nativeint_extern.to_float
      let delta = ceil(rem_lo /. 10.)
      let remainder = 10. *. delta -. rem_lo
      approx_div1
      ->sub_aux(~lo=Caml_nativeint_extern.of_float(delta), ~hi=0)
      ->unsafe_to_int64
      ->to_string ++ Caml_nativeint_extern.to_string(Caml_nativeint_extern.of_float(remainder))
    } else {
      let rem_lo = Caml_nativeint_extern.to_float(rem_lo)
      let delta = floor(rem_lo /. 10.)
      let remainder = rem_lo -. 10. *. delta
      approx_div1
      ->add_aux(~y_lo=Caml_nativeint_extern.of_float(delta), ~y_hi=0)
      ->unsafe_to_int64
      ->to_string ++ Caml_nativeint_extern.to_string(Caml_nativeint_extern.of_float(remainder))
    }
  }
}

@inline
let float_max = (a: float, b) =>
  if a > b {
    a
  } else {
    b
  }
let rec div = (self, other) =>
  switch (self, other) {
  | (_, {lo: 0, hi: 0}) => raise(Division_by_zero)
  | ({lo: 0, hi: 0}, _) => zero
  | ({lo: 0, hi: -0x8000_0000}, _) =>
    if eq(other, one) || eq(other, neg_one) {
      self
    } else if eq(other, min_int) {
      one
    } else {
      let {hi: other_hi, _} = other
      /* now |other| >= 2, so |this/other| < |MIN_VALUE| */
      let half_this = asr_(self, 1)
      let approx = lsl_(div(half_this, other), 1)
      switch approx {
      | {lo: 0, hi: 0} =>
        if other_hi < 0 {
          one
        } else {
          neg(one)
        }
      | _ =>
        let rem = sub(self, mul(other, approx))
        add(approx, div(rem, other))
      }
    }
  | (_, {lo: 0, hi: -0x8000_0000}) => zero
  | ({lo: _, hi: self_hi}, {lo: _, hi: other_hi}) =>
    if self_hi < 0 {
      if other_hi < 0 {
        div(neg(self), neg(other))
      } else {
        neg(div(neg(self), other))
      }
    } else if other_hi < 0 {
      neg(div(self, neg(other)))
    } else {
      let res = ref(zero)
      let rem = ref(self)
      /* assert false */
      while ge(rem.contents, other) {
        let approx = ref(float_max(1., Caml_float.floor(to_float(rem.contents) /. to_float(other))))
        let log2 = ceil(log(approx.contents) /. log2)
        let delta = if log2 <= 48. {
          1.
        } else {
          2. ** (log2 -. 48.)
        }
        let approxRes = ref(of_float(approx.contents))
        let approxRem = ref(mul(approxRes.contents, other))
        while (
          switch approxRem.contents {
          | {hi, _} => hi
          } < 0 || gt(approxRem.contents, rem.contents)
        ) {
          approx.contents = approx.contents -. delta
          approxRes.contents = of_float(approx.contents)
          approxRem.contents = mul(approxRes.contents, other)
        }
        if is_zero(approxRes.contents) {
          approxRes.contents = one
        }
        res.contents = add(res.contents, approxRes.contents)
        rem.contents = sub(rem.contents, approxRem.contents)
      }
      res.contents
    }
  }

let mod_ = (self, other) => sub(self, mul(div(self, other), other))

let div_mod = (self: int64, other: int64): (int64, int64) => {
  let quotient = div(unsafe_of_int64(self), unsafe_of_int64(other))
  (
    unsafe_to_int64(quotient),
    unsafe_to_int64(sub(unsafe_of_int64(self), mul(quotient, unsafe_of_int64(other)))),
  )
}

/** Note this function is unasfe here, but when combined it is actually safe
    In theory, we need do an uint_compare for [lo] components
    The thing is [uint_compare] and [int_compare] are specialised 
    to the same code when translted into js
*/
@inline
let int_compare = (x: int, y) =>
  if x < y {
    -1
  } else if x == y {
    0
  } else {
    1
  }

let compare = (self, other) => {
  let v = int_compare(self.hi, other.hi)
  if v == 0 {
    int_compare(self.lo, other.lo)
  } else {
    v
  }
}

let of_int32 = (lo: int) =>
  mk(
    ~lo,
    ~hi=if lo < 0 {
      -1
    } else {
      0
    },
  )

let to_int32 = x => lor(x.lo, 0) /* signed integer */

/* width does matter, will it be relevant to endian order? */

let to_hex = (x: int64) => {
  let {hi: x_hi, lo: x_lo} = unsafe_of_int64(x)
  let aux = (v): string =>
    Caml_string_extern.of_int(Caml_nativeint_extern.shift_right_logical(v, 0), ~base=16)

  switch (x_hi, x_lo) {
  | (0, 0) => "0"
  | (_, 0) => aux(x_hi) ++ "00000000"
  | (0, _) => aux(x_lo)
  | (_, _) =>
    let lo = aux(x_lo)
    let pad = 8 - Caml_string_extern.length(lo)
    if pad <= 0 {
      aux(x_hi) ++ lo
    } else {
      aux(x_hi) ++ (Caml_string_extern.repeat("0", pad) ++ lo)
    }
  }
}

let discard_sign = (x: int64): int64 => {
  let v = unsafe_of_int64(x)
  unsafe_to_int64(
    switch v {
    | v => {...v, hi: land(0x7fff_ffff, v.hi)}
    },
  )
}

/* >>> 0 does not change its bit representation
      it simply makes sure it is an unsigned integer
      -1 >>> 0 -> 4294967295
      Which is still (-1) if you interpret it as a signed integer
      When we do the call (new Int32Array(x[1], x[0]), it will
      convert x[0] from an unsigned integer to signed integer
   {[
     new Int32Array([-1 >>> 0])
       Int32Array(1)[-1]
   ]}
*/

let float_of_bits = (x: t): float =>
  (
    %raw(`function(lo,hi){ return (new Float64Array(new Int32Array([lo,hi]).buffer))[0]}`): (
      _,
      _,
    ) => _
  )(x.lo, x.hi)

/* let to_int32 (x : nativeint) = x |> Caml_nativeint_extern.to_int32
   in
   (*TODO:
   This should get inlined, we should apply a simple inliner in the js layer,
   the thing is its lambda representation is complex but after js layer,
   it's qutie simple
 *)
   let int32 = Int32_array.make  [| to_int32 x.lo; to_int32 x.hi |] in
   Float64_array.unsafe_get (Float64_array.fromBuffer (Int32_array.buffer int32)) 0 */

let bits_of_float: float => t = x => {
  let (lo, hi) = (%raw(`function(x){return new Int32Array(new Float64Array([x]).buffer)}`): _ => _)(
    x,
  )
  mk(~lo, ~hi)
}
