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

/*** Pseudo-random number generators (PRNG). */

/* {1 Basic functions} */

/** Initialize the generator, using the argument as a seed.
     The same seed will always yield the same sequence of numbers. */
let init: int => unit

/** Same as {!Random.init} but takes more data as seed. */
let full_init: array<int> => unit

/** Initialize the generator with a random seed chosen
   in a system-dependent way.  If [/dev/urandom] is available on
   the host machine, it is used to provide a highly random initial
   seed.  Otherwise, a less random seed is computed from system
   parameters (current time, process IDs). */
let self_init: unit => unit

/** Return 30 random bits in a nonnegative integer.
    @before 3.12.0 used a different algorithm (affects all the following
                   functions)
*/
let bits: unit => int

/** [Random.int bound] returns a random integer between 0 (inclusive)
     and [bound] (exclusive).  [bound] must be greater than 0 and less
     than 2{^30}. */
let int: int => int

/** [Random.int32 bound] returns a random integer between 0 (inclusive)
     and [bound] (exclusive).  [bound] must be greater than 0. */
let int32: Int32.t => Int32.t

/** [Random.int64 bound] returns a random integer between 0 (inclusive)
     and [bound] (exclusive).  [bound] must be greater than 0. */
let int64: Int64.t => Int64.t

/** [Random.float bound] returns a random floating-point number
   between 0 and [bound] (inclusive).  If [bound] is
   negative, the result is negative or zero.  If [bound] is 0,
   the result is 0. */
let float: float => float

/** [Random.bool ()] returns [true] or [false] with probability 0.5 each. */
let bool: unit => bool

/* {1 Advanced functions} */

module State: {
  /*** The functions from module {!State} manipulate the current state
    of the random generator explicitly.
    This allows using one or several deterministic PRNGs,
    even in a multi-threaded program, without interference from
    other parts of the program.
  */

  /** The type of PRNG states. */
  type t

  /** Create a new state and initialize it with the given seed. */
  let make: array<int> => t

  /** Create a new state and initialize it with a system-dependent
      low-entropy seed. */
  let make_self_init: unit => t

  /** Return a copy of the given state. */
  let copy: t => t

  let bits: t => int
  let int: (t, int) => int
  let int32: (t, Int32.t) => Int32.t
  let int64: (t, Int64.t) => Int64.t
  let float: (t, float) => float
  /** These functions are the same as the basic functions, except that they
      use (and update) the given PRNG state instead of the default one.
  */
  let bool: t => bool
}

/** Return the current state of the generator used by the basic functions. */
let get_state: unit => State.t

/** Set the state of the generator used by the basic functions. */
let set_state: State.t => unit
