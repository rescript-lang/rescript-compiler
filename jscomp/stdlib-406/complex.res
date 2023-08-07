/* ************************************************************************ */
/*  */
/* OCaml */
/*  */
/* Xavier Leroy, projet Cristal, INRIA Rocquencourt */
/*  */
/* Copyright 2002 Institut National de Recherche en Informatique et */
/* en Automatique. */
/*  */
/* All rights reserved.  This file is distributed under the terms of */
/* the GNU Lesser General Public License version 2.1, with the */
/* special exception on linking described in the file LICENSE. */
/*  */
/* ************************************************************************ */

/* Complex numbers */

type t = {re: float, im: float}

let zero = {re: 0.0, im: 0.0}
let one = {re: 1.0, im: 0.0}
let i = {re: 0.0, im: 1.0}

let add = (x, y) => {re: x.re +. y.re, im: x.im +. y.im}

let sub = (x, y) => {re: x.re -. y.re, im: x.im -. y.im}

let neg = x => {re: -.x.re, im: -.x.im}

let conj = x => {re: x.re, im: -.x.im}

let mul = (x, y) => {
  re: x.re *. y.re -. x.im *. y.im,
  im: x.re *. y.im +. x.im *. y.re,
}

let div = (x, y) =>
  if abs_float(y.re) >= abs_float(y.im) {
    let r = y.im /. y.re
    let d = y.re +. r *. y.im
    {
      re: (x.re +. r *. x.im) /. d,
      im: (x.im -. r *. x.re) /. d,
    }
  } else {
    let r = y.re /. y.im
    let d = y.im +. r *. y.re
    {
      re: (r *. x.re +. x.im) /. d,
      im: (r *. x.im -. x.re) /. d,
    }
  }

let inv = x => div(one, x)

let norm2 = x => x.re *. x.re +. x.im *. x.im

let norm = x => {
  /* Watch out for overflow in computing re^2 + im^2 */
  let r = abs_float(x.re)
  and i = abs_float(x.im)
  if r == 0.0 {
    i
  } else if i == 0.0 {
    r
  } else if r >= i {
    let q = i /. r
    r *. sqrt(1.0 +. q *. q)
  } else {
    let q = r /. i
    i *. sqrt(1.0 +. q *. q)
  }
}

let arg = x => atan2(x.im, x.re)

let polar = (n, a) => {re: cos(a) *. n, im: sin(a) *. n}

let sqrt = x =>
  if x.re == 0.0 && x.im == 0.0 {
    {re: 0.0, im: 0.0}
  } else {
    let r = abs_float(x.re) and i = abs_float(x.im)
    let w = if r >= i {
      let q = i /. r
      sqrt(r) *. sqrt(0.5 *. (1.0 +. sqrt(1.0 +. q *. q)))
    } else {
      let q = r /. i
      sqrt(i) *. sqrt(0.5 *. (q +. sqrt(1.0 +. q *. q)))
    }
    if x.re >= 0.0 {
      {re: w, im: 0.5 *. x.im /. w}
    } else {
      {
        re: 0.5 *. i /. w,
        im: if x.im >= 0.0 {
          w
        } else {
          -.w
        },
      }
    }
  }

let exp = x => {
  let e = exp(x.re)
  {re: e *. cos(x.im), im: e *. sin(x.im)}
}

let log = x => {re: log(norm(x)), im: atan2(x.im, x.re)}

let pow = (x, y) => exp(mul(y, log(x)))
