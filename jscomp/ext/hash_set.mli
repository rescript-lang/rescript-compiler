(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

(** Ideas are based on {!Hashtbl}, 
    however, {!Hashtbl.add} does not really optimize and has a bad semantics for {!Hash_set}, 
    This module fixes the semantics of [add].
    [remove] is not optimized since it is not used too much 
*)


type statistics = {
  num_bindings: int;
  num_buckets: int;
  max_bucket_length: int;
  bucket_histogram: int array
}

module type S =
  sig
    type key
    type t
    val create: int ->  t
    val clear : t -> unit
    val reset : t -> unit
    val copy: t -> t
    val remove:  t -> key -> unit
    val add :  t -> key -> unit
    val mem :  t -> key -> bool
    val iter: (key -> unit) ->  t -> unit
    val fold: (key -> 'b -> 'b) ->  t -> 'b -> 'b
    val length:  t -> int
    val stats:  t -> statistics
    val elements : t -> key list 
  end



module type HashedType =
  sig
    type t
    val equal: t -> t -> bool
    val hash: t -> int
  end

module Make ( H : HashedType) : (S with type key = H.t)
(** A naive t implementation on top of [hashtbl], the value is [unit]*)

type   'a t 

val create : int -> 'a t

val clear : 'a t -> unit

val reset : 'a t -> unit

val copy : 'a t -> 'a t

val add : 'a t -> 'a  -> unit
val remove : 'a t -> 'a -> unit

val mem : 'a t -> 'a -> bool

val iter : ('a -> unit) -> 'a t -> unit

val elements : 'a t -> 'a list

val length : 'a t -> int 

val stats:  'a t -> statistics
