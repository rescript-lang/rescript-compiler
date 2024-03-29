/* ************************************************************************ */
/*  */
/* OCaml */
/*  */
/* Damien Doligez, projet Para, INRIA Rocquencourt */
/*  */
/* Copyright 1997 Institut National de Recherche en Informatique et */
/* en Automatique. */
/*  */
/* All rights reserved.  This file is distributed under the terms of */
/* the GNU Lesser General Public License version 2.1, with the */
/* special exception on linking described in the file LICENSE. */
/*  */
/* ************************************************************************ */

/*** Deferred computations. */

/** A value of type ['a Lazy.t] is a deferred computation, called
   a suspension, that has a result of type ['a].  The special
   expression syntax [lazy (expr)] makes a suspension of the
   computation of [expr], without computing [expr] itself yet.
   "Forcing" the suspension will then compute [expr] and return its
   result.

   Note: [lazy_t] is the built-in type constructor used by the compiler
   for the [lazy] keyword.  You should not use it directly.  Always use
   [Lazy.t] instead.

   Note: [Lazy.force] is not thread-safe.  If you use this module in
   a multi-threaded program, you will need to add some locks.

   Note: if the program is compiled with the [-rectypes] option,
   ill-founded recursive definitions of the form [let rec x = lazy x]
   or [let rec x = lazy(lazy(...(lazy x)))] are accepted by the type-checker
   and lead, when forced, to ill-formed values that trigger infinite
   loops in the garbage collector and other parts of the run-time system.
   Without the [-rectypes] option, such ill-founded recursive definitions
   are rejected by the type-checker.
*/
type t<'a> = lazy_t<'a>

exception Undefined

/* val force : 'a t -> 'a */
/** [force x] forces the suspension [x] and returns its result.
   If [x] has already been forced, [Lazy.force x] returns the
   same value again without recomputing it.  If it raised an exception,
   the same exception is raised again.
   Raise {!Undefined} if the forcing of [x] tries to force [x] itself
   recursively.
*/
external force: t<'a> => 'a = "%lazy_force"

/** [force_val x] forces the suspension [x] and returns its
    result.  If [x] has already been forced, [force_val x]
    returns the same value again without recomputing it.
    Raise {!Undefined} if the forcing of [x] tries to force [x] itself
    recursively.
    If the computation of [x] raises an exception, it is unspecified
    whether [force_val x] raises the same exception or {!Undefined}.
*/
let force_val: t<'a> => 'a

/** [from_fun f] is the same as [lazy (f ())] but slightly more efficient.

    [from_fun] should only be used if the function [f] is already defined.
    In particular it is always less efficient to write
    [from_fun (fun () -> expr)] than [lazy expr].

    @since 4.00.0 */
let from_fun: (unit => 'a) => t<'a>

/** [from_val v] returns an already-forced suspension of [v].
    This is for special purposes only and should not be confused with
    [lazy (v)].
    @since 4.00.0 */
let from_val: 'a => t<'a>

/** [is_val x] returns [true] if [x] has already been forced and
    did not raise an exception.
    @since 4.00.0 */
let is_val: t<'a> => bool

@deprecated("Use Lazy.from_fun instead.") /** @deprecated synonym for [from_fun]. */
let lazy_from_fun: (unit => 'a) => t<'a>

@deprecated("Use Lazy.from_val instead.") /** @deprecated synonym for [from_val]. */
let lazy_from_val: 'a => t<'a>

@deprecated("Use Lazy.is_val instead.") /** @deprecated synonym for [is_val]. */
let lazy_is_val: t<'a> => bool
