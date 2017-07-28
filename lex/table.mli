(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Luc Maranget, projet Moscova, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Table used for code emission, ie extensible arrays *)
type 'a t

val create : 'a -> 'a t

val emit : 'a t -> 'a -> unit

val iter : 'a t -> ('a -> unit) -> unit

val trim : 'a t -> 'a array


exception Error

val get : 'a t -> int -> 'a



val size : 'a t -> int
