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

/*** Registering OCaml values with the C runtime.

   This module allows OCaml values to be registered with the C runtime
   under a symbolic name, so that C code can later call back registered
   OCaml functions, or raise registered OCaml exceptions.
*/

/** [Callback.register n v] registers the value [v] under
   the name [n]. C code can later retrieve a handle to [v]
   by calling [caml_named_value(n)]. */
let register: (string, 'a) => unit

/** [Callback.register_exception n exn] registers the
   exception contained in the exception value [exn]
   under the name [n]. C code can later retrieve a handle to
   the exception by calling [caml_named_value(n)]. The exception
   value thus obtained is suitable for passing as first argument
   to [raise_constant] or [raise_with_arg]. */
let register_exception: (string, exn) => unit
