/* ************************************************************************ */
/*  */
/* OCaml */
/*  */
/* Xavier Leroy, projet Cristal, INRIA Rocquencourt */
/*  */
/* Copyright 2004 Institut National de Recherche en Informatique et */
/* en Automatique. */
/*  */
/* All rights reserved.  This file is distributed under the terms of */
/* the GNU Lesser General Public License version 2.1, with the */
/* special exception on linking described in the file LICENSE. */
/*  */
/* ************************************************************************ */

/*** Run-time support for recursive modules.
    All functions in this module are for system use only, not for the
    casual user. */

type rec shape =
  | Function
  | Lazy
  | Class
  | Module(array<shape>)
  | Value(Obj.t)
