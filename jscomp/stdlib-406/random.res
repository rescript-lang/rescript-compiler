/* ************************************************************************ */
/*  */
/* OCaml */
/*  */
/* Damien Doligez, projet Para, INRIA Rocquencourt */
/*  */
/* Copyright 1996 Institut National de Recherche en Informatique et */
/* en Automatique. */
/*  */
/* All rights reserved.  This file is distributed under the terms of */
/* the GNU Lesser General Public License version 2.1, with the */
/* special exception on linking described in the file LICENSE. */
/*  */
/* ************************************************************************ */

/* Pseudo-random number generator
   This is a lagged-Fibonacci F(55, 24, +) with a modified addition
   function to enhance the mixing of bits.
   If we use normal addition, the low-order bit fails tests 1 and 7
   of the Diehard test suite, and bits 1 and 2 also fail test 7.
   If we use multiplication as suggested by Marsaglia, it doesn't fare
   much better.
   By mixing the bits of one of the numbers before addition (XOR the
   5 high-order bits into the low-order bits), we get a generator that
   passes all the Diehard tests.
*/

let random_seed: unit => array<int> = _ => {
  let seed: int = %raw("Math.floor(Math.random()*0x7fffffff)")
  [seed]
}

module State = {
  type t = {st: array<int>, mutable idx: int}

  let new_state = () => {st: Array.make(55, 0), idx: 0}
  let assign = (st1, st2) => {
    Array.blit(st2.st, 0, st1.st, 0, 55)
    st1.idx = st2.idx
  }

  let full_init = (s, seed) => {
    let combine = (accu, x) => Digest.string(accu ++ string_of_int(x))
    let extract = d =>
      Char.code(String.get(d, 0)) +
      lsl(Char.code(String.get(d, 1)), 8) +
      lsl(Char.code(String.get(d, 2)), 16) +
      lsl(Char.code(String.get(d, 3)), 24)

    let seed = if Array.length(seed) == 0 {
      [0]
    } else {
      seed
    }
    let l = Array.length(seed)
    for i in 0 to 54 {
      s.st[i] = i
    }
    let accu = ref("x")
    for i in 0 to 54 + max(55, l) {
      let j = mod(i, 55)
      let k = mod(i, l)
      accu := combine(accu.contents, seed[k])
      s.st[j] = land(lxor(s.st[j], extract(accu.contents)), 0x3FFFFFFF) /* PR#5575 */
    }
    s.idx = 0
  }

  let make = seed => {
    let result = new_state()
    full_init(result, seed)
    result
  }

  let make_self_init = () => make(random_seed())

  let copy = s => {
    let result = new_state()
    assign(result, s)
    result
  }

  /* Returns 30 random bits as an integer 0 <= x < 1073741824 */
  let bits = s => {
    s.idx = mod(s.idx + 1, 55)
    let curval = s.st[s.idx]
    let newval = s.st[mod(s.idx + 24, 55)] + lxor(curval, land(lsr(curval, 25), 0x1F))
    let newval30 = land(newval, 0x3FFFFFFF) /* PR#5575 */
    s.st[s.idx] = newval30
    newval30
  }

  let rec intaux = (s, n) => {
    let r = bits(s)
    let v = mod(r, n)
    if r - v > 0x3FFFFFFF - n + 1 {
      intaux(s, n)
    } else {
      v
    }
  }

  let int = (s, bound) =>
    if bound > 0x3FFFFFFF || bound <= 0 {
      invalid_arg("Random.int")
    } else {
      intaux(s, bound)
    }

  let rec int32aux = (s, n) => {
    let b1 = Int32.of_int(bits(s))
    let b2 = Int32.shift_left(Int32.of_int(land(bits(s), 1)), 30)
    let r = Int32.logor(b1, b2)
    let v = Int32.rem(r, n)
    if Int32.sub(r, v) > Int32.add(Int32.sub(Int32.max_int, n), 1l) {
      int32aux(s, n)
    } else {
      v
    }
  }

  let int32 = (s, bound) =>
    if bound <= 0l {
      invalid_arg("Random.int32")
    } else {
      int32aux(s, bound)
    }

  let rec int64aux = (s, n) => {
    let b1 = Int64.of_int(bits(s))
    let b2 = Int64.shift_left(Int64.of_int(bits(s)), 30)
    let b3 = Int64.shift_left(Int64.of_int(land(bits(s), 7)), 60)
    let r = Int64.logor(b1, Int64.logor(b2, b3))
    let v = Int64.rem(r, n)
    if Int64.sub(r, v) > Int64.add(Int64.sub(Int64.max_int, n), 1L) {
      int64aux(s, n)
    } else {
      v
    }
  }

  let int64 = (s, bound) =>
    if bound <= 0L {
      invalid_arg("Random.int64")
    } else {
      int64aux(s, bound)
    }

  /* Returns a float 0 <= x <= 1 with at most 60 bits of precision. */
  let rawfloat = s => {
    let scale = 1073741824.0 /* 2^30 */
    and r1 = Pervasives.float(bits(s))
    and r2 = Pervasives.float(bits(s))
    (r1 /. scale +. r2) /. scale
  }

  let float = (s, bound) => rawfloat(s) *. bound

  let bool = s => land(bits(s), 1) == 0
}

/* This is the state you get with [init 27182818] and then applying
 the "land 0x3FFFFFFF" filter to them.  See #5575, #5793, #5977. */
let default = {
  State.st: [
    0x3ae2522b,
    0x1d8d4634,
    0x15b4fad0,
    0x18b14ace,
    0x12f8a3c4,
    0x3b086c47,
    0x16d467d6,
    0x101d91c7,
    0x321df177,
    0x0176c193,
    0x1ff72bf1,
    0x1e889109,
    0x0b464b18,
    0x2b86b97c,
    0x0891da48,
    0x03137463,
    0x085ac5a1,
    0x15d61f2f,
    0x3bced359,
    0x29c1c132,
    0x3a86766e,
    0x366d8c86,
    0x1f5b6222,
    0x3ce1b59f,
    0x2ebf78e1,
    0x27cd1b86,
    0x258f3dc3,
    0x389a8194,
    0x02e4c44c,
    0x18c43f7d,
    0x0f6e534f,
    0x1e7df359,
    0x055d0b7e,
    0x10e84e7e,
    0x126198e4,
    0x0e7722cb,
    0x1cbede28,
    0x3391b964,
    0x3d40e92a,
    0x0c59933d,
    0x0b8cd0b7,
    0x24efff1c,
    0x2803fdaa,
    0x08ebc72e,
    0x0f522e32,
    0x05398edc,
    0x2144a04c,
    0x0aef3cbd,
    0x01ad4719,
    0x35b93cd6,
    0x2a559d4f,
    0x1e6fd768,
    0x26e27f36,
    0x186f18c3,
    0x2fbf967a,
  ],
  State.idx: 0,
}

let bits = () => State.bits(default)
let int = bound => State.int(default, bound)
let int32 = bound => State.int32(default, bound)

let int64 = bound => State.int64(default, bound)
let float = scale => State.float(default, scale)
let bool = () => State.bool(default)

let full_init = seed => State.full_init(default, seed)
let init = seed => State.full_init(default, [seed])
let self_init = () => full_init(random_seed())

/* Manipulating the current state. */

let get_state = () => State.copy(default)
let set_state = s => State.assign(default, s)

/* *******************

(* Test functions.  Not included in the library.
   The [chisquare] function should be called with n > 10r.
   It returns a triple (low, actual, high).
   If low <= actual <= high, the [g] function passed the test,
   otherwise it failed.

  Some results:

init 27182818; chisquare int 100000 1000
init 27182818; chisquare int 100000 100
init 27182818; chisquare int 100000 5000
init 27182818; chisquare int 1000000 1000
init 27182818; chisquare int 100000 1024
init 299792643; chisquare int 100000 1024
init 14142136; chisquare int 100000 1024
init 27182818; init_diff 1024; chisquare diff 100000 1024
init 27182818; init_diff 100; chisquare diff 100000 100
init 27182818; init_diff2 1024; chisquare diff2 100000 1024
init 27182818; init_diff2 100; chisquare diff2 100000 100
init 14142136; init_diff2 100; chisquare diff2 100000 100
init 299792643; init_diff2 100; chisquare diff2 100000 100
- : float * float * float = (936.754446796632465, 997.5, 1063.24555320336754)
# - : float * float * float = (80., 89.7400000000052387, 120.)
# - : float * float * float = (4858.57864376269, 5045.5, 5141.42135623731)
# - : float * float * float =
(936.754446796632465, 944.805999999982305, 1063.24555320336754)
# - : float * float * float = (960., 1019.19744000000355, 1088.)
# - : float * float * float = (960., 1059.31776000000536, 1088.)
# - : float * float * float = (960., 1039.98463999999512, 1088.)
# - : float * float * float = (960., 1054.38207999999577, 1088.)
# - : float * float * float = (80., 90.096000000005, 120.)
# - : float * float * float = (960., 1076.78720000000612, 1088.)
# - : float * float * float = (80., 85.1760000000067521, 120.)
# - : float * float * float = (80., 85.2160000000003492, 120.)
# - : float * float * float = (80., 80.6220000000030268, 120.)

*)

(* Return the sum of the squares of v[i0,i1[ *)
let rec sumsq v i0 i1 =
  if i0 >= i1 then 0.0
  else if i1 = i0 + 1 then Pervasives.float v.(i0) *. Pervasives.float v.(i0)
  else sumsq v i0 ((i0+i1)/2) +. sumsq v ((i0+i1)/2) i1


let chisquare g n r =
  if n <= 10 * r then invalid_arg "chisquare";
  let f = Array.make r 0 in
  for i = 1 to n do
    let t = g r in
    f.(t) <- f.(t) + 1
  done;
  let t = sumsq f 0 r
  and r = Pervasives.float r
  and n = Pervasives.float n in
  let sr = 2.0 *. sqrt r in
  (r -. sr,   (r *. t /. n) -. n,   r +. sr)


(* This is to test for linear dependencies between successive random numbers.
*)
let st = ref 0
let init_diff r = st := int r
let diff r =
  let x1 = !st
  and x2 = int r
  in
  st := x2;
  if x1 >= x2 then
    x1 - x2
  else
    r + x1 - x2


let st1 = ref 0
and st2 = ref 0


(* This is to test for quadratic dependencies between successive random
   numbers.
*)
let init_diff2 r = st1 := int r; st2 := int r
let diff2 r =
  let x1 = !st1
  and x2 = !st2
  and x3 = int r
  in
  st1 := x2;
  st2 := x3;
  (x3 - x2 - x2 + x1 + 2*r) mod r


********************/
