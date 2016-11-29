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


type ('key, 'a) t =
  | Empty
  | Node of ('key, 'a) t * 'key * 'a * ('key, 'a) t * int


val cardinal : ('a, 'b) t -> int

val bindings : ('a, 'b) t -> ('a * 'b) list

val height : ('a, 'b) t -> int

val create : ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t -> ('a, 'b) t

val singleton : 'a -> 'b -> ('a, 'b) t

val bal : ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t -> ('a, 'b) t

val empty : ('a, 'b) t

val is_empty : ('a, 'b) t -> bool


val choose : ('a, 'b) t -> 'a * 'b

val remove_min_binding : ('a, 'b) t -> ('a, 'b) t

val iter : ('a -> 'b -> 'c) -> ('a, 'b) t -> unit

val map : ('a -> 'b) -> ('c, 'a) t -> ('c, 'b) t

val mapi : ('a -> 'b -> 'c) -> ('a, 'b) t -> ('a, 'c) t

val fold : ('a -> 'b -> 'c -> 'c) -> ('a, 'b) t -> 'c -> 'c

val for_all : ('a -> 'b -> bool) -> ('a, 'b) t -> bool

val exists : ('a -> 'b -> bool) -> ('a, 'b) t -> bool

val join : ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t -> ('a, 'b) t

val concat : ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t

val concat_or_join :
  ('a, 'b) t -> 'a -> 'b option -> ('a, 'b) t -> ('a, 'b) t
    
val filter : ('a -> 'b -> bool) -> ('a, 'b) t -> ('a, 'b) t

val partition : ('a -> 'b -> bool) -> ('a, 'b) t -> ('a, 'b) t * ('a, 'b) t


(*******************************************************************)
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
