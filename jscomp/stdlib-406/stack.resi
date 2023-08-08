/* ************************************************************************ */
/*  */
/* OCaml */
/*  */
/* Xavier Leroy, projet Cristal, INRIA Rocquencourt */
/*  */
/* Copyright 1996 Institut National de Recherche en Informatique et */
/* en Automatique. */
/*  */
/* All rights reserved.  This file is distributed under the terms of */
/* the GNU Lesser General Public License version 2.1, with the */
/* special exception on linking described in the file LICENSE. */
/*  */
/* ************************************************************************ */

/*** Last-in first-out stacks.

   This module implements stacks (LIFOs), with in-place modification.
*/

/** The type of stacks containing elements of type ['a]. */
type t<'a>

/** Raised when {!Stack.pop} or {!Stack.top} is applied to an empty stack. */ exception Empty

/** Return a new stack, initially empty. */
let create: unit => t<'a>

/** [push x s] adds the element [x] at the top of stack [s]. */
let push: ('a, t<'a>) => unit

/** [pop s] removes and returns the topmost element in stack [s],
   or raises {!Empty} if the stack is empty. */
let pop: t<'a> => 'a

/** [top s] returns the topmost element in stack [s],
   or raises {!Empty} if the stack is empty. */
let top: t<'a> => 'a

/** Discard all elements from a stack. */
let clear: t<'a> => unit

/** Return a copy of the given stack. */
let copy: t<'a> => t<'a>

/** Return [true] if the given stack is empty, [false] otherwise. */
let is_empty: t<'a> => bool

/** Return the number of elements in a stack. Time complexity O(1) */
let length: t<'a> => int

/** [iter f s] applies [f] in turn to all elements of [s],
   from the element at the top of the stack to the element at the
   bottom of the stack. The stack itself is unchanged. */
let iter: ('a => unit, t<'a>) => unit

/** [fold f accu s] is [(f (... (f (f accu x1) x2) ...) xn)]
    where [x1] is the top of the stack, [x2] the second element,
    and [xn] the bottom element. The stack is unchanged.
    @since 4.03 */
let fold: (('b, 'a) => 'b, 'b, t<'a>) => 'b
