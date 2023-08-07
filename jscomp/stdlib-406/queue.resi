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

/*** First-in first-out queues.

   This module implements queues (FIFOs), with in-place modification.

   {b Warning} This module is not thread-safe: each {!Queue.t} value
   must be protected from concurrent access (e.g. with a [Mutex.t]).
   Failure to do so can lead to a crash.
*/

/** The type of queues containing elements of type ['a]. */
type t<'a>

/** Raised when {!Queue.take} or {!Queue.peek} is applied to an empty queue. */ exception Empty

/** Return a new queue, initially empty. */
let create: unit => t<'a>

/** [add x q] adds the element [x] at the end of the queue [q]. */
let add: ('a, t<'a>) => unit

/** [push] is a synonym for [add]. */
let push: ('a, t<'a>) => unit

/** [take q] removes and returns the first element in queue [q],
   or raises {!Empty} if the queue is empty. */
let take: t<'a> => 'a

/** [pop] is a synonym for [take]. */
let pop: t<'a> => 'a

/** [peek q] returns the first element in queue [q], without removing
   it from the queue, or raises {!Empty} if the queue is empty. */
let peek: t<'a> => 'a

/** [top] is a synonym for [peek]. */
let top: t<'a> => 'a

/** Discard all elements from a queue. */
let clear: t<'a> => unit

/** Return a copy of the given queue. */
let copy: t<'a> => t<'a>

/** Return [true] if the given queue is empty, [false] otherwise. */
let is_empty: t<'a> => bool

/** Return the number of elements in a queue. */
let length: t<'a> => int

/** [iter f q] applies [f] in turn to all elements of [q],
   from the least recently entered to the most recently entered.
   The queue itself is unchanged. */
let iter: ('a => unit, t<'a>) => unit

/** [fold f accu q] is equivalent to [List.fold_left f accu l],
   where [l] is the list of [q]'s elements. The queue remains
   unchanged. */
let fold: (('b, 'a) => 'b, 'b, t<'a>) => 'b

/** [transfer q1 q2] adds all of [q1]'s elements at the end of
   the queue [q2], then clears [q1]. It is equivalent to the
   sequence [iter (fun x -> add x q2) q1; clear q1], but runs
   in constant time. */
let transfer: (t<'a>, t<'a>) => unit
