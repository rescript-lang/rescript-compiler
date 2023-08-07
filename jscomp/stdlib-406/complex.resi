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

/*** Complex numbers.

    This module provides arithmetic operations on complex numbers.
    Complex numbers are represented by their real and imaginary parts
    (cartesian representation).  Each part is represented by a
    double-precision floating-point number (type [float]). */

/** The type of complex numbers.  [re] is the real part and [im] the
    imaginary part. */
type t = {re: float, im: float}

/** The complex number [0]. */
let zero: t

/** The complex number [1]. */
let one: t

/** The complex number [i]. */
let i: t

/** Unary negation. */
let neg: t => t

/** Conjugate: given the complex [x + i.y], returns [x - i.y]. */
let conj: t => t

/** Addition */
let add: (t, t) => t

/** Subtraction */
let sub: (t, t) => t

/** Multiplication */
let mul: (t, t) => t

/** Multiplicative inverse ([1/z]). */
let inv: t => t

/** Division */
let div: (t, t) => t

/** Square root.  The result [x + i.y] is such that [x > 0] or
    [x = 0] and [y >= 0].
    This function has a discontinuity along the negative real axis. */
let sqrt: t => t

/** Norm squared: given [x + i.y], returns [x^2 + y^2]. */
let norm2: t => float

/** Norm: given [x + i.y], returns [sqrt(x^2 + y^2)]. */
let norm: t => float

/** Argument.  The argument of a complex number is the angle
    in the complex plane between the positive real axis and a line
    passing through zero and the number.  This angle ranges from
    [-pi] to [pi].  This function has a discontinuity along the
    negative real axis. */
let arg: t => float

/** [polar norm arg] returns the complex having norm [norm]
    and argument [arg]. */
let polar: (float, float) => t

/** Exponentiation.  [exp z] returns [e] to the [z] power. */
let exp: t => t

/** Natural logarithm (in base [e]). */
let log: t => t

/** Power function.  [pow z1 z2] returns [z1] to the [z2] power. */
let pow: (t, t) => t
