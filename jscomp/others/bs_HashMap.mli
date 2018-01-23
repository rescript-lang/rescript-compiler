(* Copyright (C) 2018 Authors of BuckleScript
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




type ('a,'b,'id) t 
  
(** The type of hash tables from type ['a] to type ['b]. *)

val create: ('a,'id) Bs_Hash.t -> int -> ('a,'b,'id) t
(*TODO: allow randomization for security *)

val clear: ('a, 'b, 'id) t -> unit
(** Empty a hash table. Use [reset] instead of [clear] to shrink the
    size of the bucket table to its initial size. *)



val setDone: ('a, 'b, 'id) t -> 'a -> 'b -> unit
val set: ('a, 'b, 'id) t -> 'a -> 'b -> ('a, 'b, 'id) t
val copy: ('a, 'b, 'id) t -> ('a, 'b, 'id) t
    
val get:  
  ('a, 'b, 'id) t -> 'a -> 'b option

  
val has: ('a, 'b, 'id) t -> 'a -> bool
(** [has tbl x] checks if [x] is bound in [tbl]. *)

val removeDone:
  ('a, 'b, 'id) t -> 'a ->
  unit
val remove:
  ('a, 'b, 'id) t -> 'a ->
  ('a, 'b, 'id) t

val forEach: ('a, 'b, 'id) t -> ('a -> 'b -> unit [@bs]) -> unit
(** [forEach tbl f] applies [f] to all bindings in table [tbl].
    [f] receives the key as first argument, and the associated value
    as second argument. Each binding is presented exactly once to [f].

    If the hash table was created in non-randomized mode, the order
    in which the bindings are enumerated is reproducible between
    successive runs of the program. *)


val reduce: ('a, 'b, 'id) t -> 'c -> ('c -> 'a -> 'b ->  'c [@bs]) ->  'c
(** [Hashtbl.reduce  tbl init f] computes
    [(f kN dN ... (f k1 d1 init)...)],
    where [k1 ... kN] are the keys of all bindings in [tbl],
    and [d1 ... dN] are the associated values.
    Each binding is presented exactly once to [f].

    The order in which the bindings are passed to [f] is unspecified.
    However, if the table contains several bindings for the same key,
    they are passed to [f] in reverse order of introduction, that is,
    the most recent binding is passed first.

    If the hash table was created in non-randomized mode, the order
    in which the bindings are enumerated is reproducible between
    successive runs of the program, and even between minor versions
    of OCaml.  For randomized hash tables, the order of enumeration
    is entirely random. *)


val filterMapDone: ('a, 'b, 'id) t -> ('a -> 'b -> 'b option [@bs]) ->  unit
val filterMap: ('a, 'b, 'id) t -> ('a -> 'b -> 'b option [@bs]) -> ('a, 'b, 'id) t


val size: ('a, 'b, 'id) t -> int  
(** [Hashtbl.length tbl] returns the number of bindings in [tbl].
    It takes constant time.  Multiple bindings are counted once each, so
    [Hashtbl.length] gives the number of times [Hashtbl.iter] calls its
    first argument. *)



val logStats : _ t -> unit
(** [Hashtbl.stats tbl] returns statistics about the table [tbl]:
    number of buckets, size of the biggest bucket, distribution of
    buckets by size.
    @since 4.00.0 *)




val toArray: ('a, 'b, 'id) t -> ('a * 'b) array 




val ofArray:   
  dict:('a,'id) Bs_Hash.t -> 
  ('a * 'b) array -> 
  ('a, 'b, 'id) t 
val mergeArrayDone: ('a, 'b, 'id) t -> ('a * 'b) array -> unit
val mergeArray: ('a, 'b, 'id) t -> ('a * 'b) array -> ('a, 'b, 'id) t

val keysToArray:    
    ('a,'b,'id) t -> 'a array    
val valuesToArray:    
    ('a,'b,'id) t -> 'b array    

(****************************************************************************)
      
type ('a, 'b, 'id) t0
val getData: ('k,'v,'id) t  -> ('k,'v,'id) t0
val getDict: ('k,'v,'id) t  -> ('k,'id) Bs_Hash.t
val packDictData: dict:('k, 'id) Bs_Hash.t -> data:('k, 'v, 'id) t0 -> ('k, 'v, 'id) t
val create0: int -> ('a, 'b, 'id) t0
val clear0: ('a, 'b, 'id) t0 -> unit
val setDone0 : 
    hash:('a,'id) Bs_Hash.hash ->
    eq:('a,'id) Bs_Hash.eq -> 
     ('a,'b,'id) t0 -> 'a ->
    'b -> unit
val get0: 
  hash:('a,'id) Bs_Hash.hash  -> 
  eq:('a,'id) Bs_Hash.eq -> 
  ('a, 'b, 'id) t0 ->
  'a ->
  'b option

val has0: 
  hash:('a,'id) Bs_Hash.hash  -> 
  eq:('a,'id) Bs_Hash.eq -> 
  ('a, 'b, 'id) t0 ->
  'a ->
  bool
  
val remove0: 
  hash:('a,'id) Bs_Hash.hash  -> 
  eq:('a,'id) Bs_Hash.eq -> 
  ('a, 'b, 'id) t0 ->
  'a ->
  unit

val forEach0: ('a, 'b, 'id) t0 -> ('a -> 'b -> unit [@bs]) -> unit
val reduce0: ('a, 'b, 'id) t0 -> 'c -> ('c -> 'a -> 'b ->  'c [@bs]) -> 'c
val filterMapDone0: ('a, 'b, 'id) t0 -> ('a -> 'b -> 'b option [@bs]) -> unit
val size0: ('a, 'b, 'id) t0 -> int
val logStats0: ('a, 'b, 'id) t0 -> unit
val toArray0: ('a, 'b, 'id) t0 -> ('a * 'b) array
val ofArray0: 
  hash:('a,'id) Bs_Hash.hash  -> 
  eq:('a,'id) Bs_Hash.eq -> 
  ('a * 'b) array -> 
  ('a, 'b, 'id) t0      
val mergeArray0: 
  ('a, 'b, 'id) t0 ->
  ('a * 'b) array ->
  hash:('a,'id) Bs_Hash.hash  -> 
  eq:('a,'id) Bs_Hash.eq -> 
  ('a, 'b, 'id) t0
    
val keysToArray0:    
    ('a,'b,'id) t0 -> 'a array
val valuesToArray0:    
    ('a,'b,'id) t0 -> 'b array


