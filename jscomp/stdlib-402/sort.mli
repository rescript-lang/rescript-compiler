(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(** Sorting and merging lists.

   @deprecated This module is obsolete and exists only for backward
   compatibility.
   The sorting functions in {!Array} and {!List} should be used instead.
   The new functions are faster and use less memory.
*)

val list : ('a -> 'a -> bool) -> 'a list -> 'a list
  [@@ocaml.deprecated "Use List.sort instead."]
(** Sort a list in increasing order according to an ordering predicate.
   The predicate should return [true] if its first argument is
   less than or equal to its second argument. *)

val array : ('a -> 'a -> bool) -> 'a array -> unit
  [@@ocaml.deprecated "Use Array.sort instead."]
(** Sort an array in increasing order according to an
   ordering predicate.
   The predicate should return [true] if its first argument is
   less than or equal to its second argument.
   The array is sorted in place. *)

val merge : ('a -> 'a -> bool) -> 'a list -> 'a list -> 'a list
  [@@ocaml.deprecated "Use List.merge instead."]
(** Merge two lists according to the given predicate.
   Assuming the two argument lists are sorted according to the
   predicate, [merge] returns a sorted list containing the elements
   from the two lists. The behavior is undefined if the two
   argument lists were not sorted. *)
