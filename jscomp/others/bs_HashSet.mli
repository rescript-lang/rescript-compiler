type ('a, 'id) t0

type ('a, 'id) t = 
  (('a, 'id) Bs_Hash.t,
   ('a, 'id) t0) Bs_Bag.bag 

(** The type of hash tables from type ['a] to type ['b]. *)

val create0 : int -> ('a, 'id) t0
val create : ('a,'id) Bs_Hash.t -> int -> ('a, 'id) t


val clear0 : ('a, 'id) t0 -> unit
val clear : ('a, 'id) t -> unit
(** Empty a hash table. Use [reset] instead of [clear] to shrink the
    size of the bucket table to its initial size. *)

val reset0 : ('a, 'id) t0 -> unit
val reset : ('a, 'id) t -> unit
(** Empty a hash table and shrink the size of the bucket table
    to its initial size.
    @since 4.00.0 *)



val add0 :
  hash:('a,'id) Bs_Hash.hash ->
  eq:('a,'id) Bs_Hash.eq -> 
  ('a,'id) t0 -> 'a ->  unit
val add : ('a, 'id) t -> 'a -> unit

val mem0: 
  hash:('a,'id) Bs_Hash.hash  -> 
  eq:('a,'id) Bs_Hash.eq -> 
  ('a, 'id) t0 -> 'a -> bool
val mem:  
  ('a, 'id) t -> 'a -> bool


val remove0: 
  hash:('a,'id) Bs_Hash.hash  -> 
  eq:('a,'id) Bs_Hash.eq -> 
  ('a, 'id) t0 -> 'a -> unit
val remove:
  ('a, 'id) t -> 'a -> unit



val iter0 : ('a, 'id) t0 -> ('a -> unit [@bs]) ->  unit
val iter : ('a, 'id) t -> ('a  -> unit [@bs]) ->  unit
(** 
    If the hash table was created in non-randomized mode, the order
    in which the bindings are enumerated is reproducible between
    successive runs of the program, and even between minor versions
    of OCaml.  For randomized hash tables, the order of enumeration
    is entirely random. *)

val fold0 : ('a, 'id) t0 -> 'c -> ('a -> 'c -> 'c [@bs]) ->  'c
val fold : ('a, 'id) t -> 'c -> ('a  -> 'c -> 'c [@bs]) -> 'c
(**
    If the hash table was created in non-randomized mode, the order
    in which the bindings are enumerated is reproducible between
    successive runs of the program, and even between minor versions
    of OCaml.  For randomized hash tables, the order of enumeration
    is entirely random. *)


val length0 : ('a, 'id) t0 -> int
val length  : ('a, 'id) t -> int  

val logStats0 : ('a, 'id) t0 -> unit
val logStats : _ t -> unit
(** [Hashtbl.stats tbl] returns statistics about the table [tbl]:
    number of buckets, size of the biggest bucket, distribution of
    buckets by size.
    @since 4.00.0 *)

val toArray0 : ('a,'id) t0 -> 'a array
val toArray : ('a,'id) t -> 'a array 

val ofArray0 : 
  hash:('a,'id) Bs_Hash.hash  -> 
  eq:('a,'id) Bs_Hash.eq -> 
  'a array -> 
  ('a, 'id) t0      

val ofArray :   
  dict:('a,'id) Bs_Hash.t -> 
  'a array -> 
  ('a,'id) t 

val addArray0 : 
  hash:('a,'id) Bs_Hash.hash  -> 
  eq:('a,'id) Bs_Hash.eq -> 
  ('a,'id) t0 -> 'a array -> unit     

val addArray:   
   ('a,'id) t -> 'a array -> unit   