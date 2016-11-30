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

(*******************************************************************)
type ('key,'value) t =  ('key,'value) Bal_map_common.t 
val add : 'a -> 'b -> ('a, 'b) t -> ('a, 'b) t
val find : 'a -> ('a, 'b) t -> 'b
val mem : 'a -> ('a, 'b) t -> bool
val remove : 'a -> ('a, 'b) t -> ('a, 'b) t
val split : 'a -> ('a, 'b) t -> ('a, 'b) t * 'b option * ('a, 'b) t
val merge :
  ('a -> 'b option -> 'c option -> 'd option) ->
  ('a, 'b) t -> ('a, 'c) t -> ('a, 'd) t
val compare : ('a -> 'b -> int) -> ('c, 'a) t -> ('c, 'b) t -> int
val equal : ('a -> 'b -> bool) -> ('c, 'a) t -> ('c, 'b) t -> bool
