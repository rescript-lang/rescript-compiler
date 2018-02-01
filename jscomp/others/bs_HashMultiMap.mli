



type ('key,'value,'id) t 
  
(** The type of hash tables from type ['a] to type ['b]. *)

type ('a, 'id) eq = ('a, 'id) Bs_Dict.eq
type ('a, 'id) hash = ('a, 'id) Bs_Dict.hash    
type ('a, 'id) dict = ('a, 'id) Bs_Dict.hashable

val make :  int -> dict: ('a, 'id) dict ->  ('a,'b,'id) t
(** [make n ~dict] creates a new, empty hash table, with
    initial size [n].  For best results, [n] should be on the
    order of the expected number of elements that will be in
    the table.  The table grows as needed, so [n] is just an
    initial guess.
*)


val clear : ('a, 'b, 'id) t -> unit
(** Empty a hash table.  *)

val isEmpty: _ t -> bool 


val add: ('a, 'b, 'id) t -> 'a -> 'b -> unit
(** [add tbl x y] adds a binding of [x] to [y] in table [tbl].
    Previous bindings for [x] are not removed, but simply
    hidden. That is, after performing [remove tbl x],
    the previous binding for [x], if any, is restored.
    (Same behavior as with association lists.) *)

 val replace: ('a, 'b, 'id) t -> 'a -> 'b -> unit
(** [replace tbl x y] replaces the current binding of [x]
    in [tbl] by a binding of [x] to [y].  If [x] is unbound in [tbl],
    a binding of [x] to [y] is added to [tbl].
    This is functionally equivalent to [remove tbl x]
    followed by [add tbl x y]. *)

val replaceBy: 
    ('key, 'value, 'id) t -> 
    'key -> 
    'value -> 
    eq:('key,'id) eq ->
    hash:('key,'id) hash -> 
    unit 
(** [replaceBy tbl k v ~eq ~hash] the same as 
    [replace tbl k v] except that [eq] and [hash]
    is  supplied directly so it can be batched (slightly faster)
*)

val copy: ('key, 'value, 'id) t -> ('key, 'value, 'id) t  

val get:  ('key, 'value, 'id) t -> 'key -> 'value option


val has:  ('key, 'value, 'id) t -> 'key -> bool
(** [has tbl x] checks if [x] is bound in [tbl]. *)

  
val remove: ('key, 'value, 'id) t -> 'key -> unit
(** [remove tbl x] removes the current binding of [x] in [tbl],
    restoring the previous binding if it exists.
    It does nothing if [x] is not bound in [tbl]. *)

val forEach : ('a, 'b, 'id) t -> ('a -> 'b -> unit [@bs]) -> unit
(** [forEach f tbl] applies [f] to all bindings in table [tbl].
    [f] receives the key as first argument, and the associated value
    as second argument. Each binding is presented exactly once to [f].
*)


val reduce : ('a, 'b, 'id) t -> 'c -> ('c -> 'a -> 'b ->  'c [@bs]) ->  'c
(** [reduce f tbl init] computes
    [(f kN dN ... (f k1 d1 init)...)],
    where [k1 ... kN] are the keys of all bindings in [tbl],
    and [d1 ... dN] are the associated values.
    Each binding is presented exactly once to [f].

    The order in which the bindings are passed to [f] is unspecified.
    However, if the table contains several bindings for the same key,
    they are passed to [f] in reverse order of introduction, that is,
    the most recent binding is passed first.
*)


val keepMapInPlace : ('a, 'b, 'id) t -> ('a -> 'b -> 'b option [@bs]) ->  unit
  
val size  : _ t -> int  
(** [size tbl] returns the number of bindings in [tbl].
    It takes constant time.  Multiple bindings are counted once each, so
    [size] gives the number of times [forEach] calls its
    first argument. *)



val toArray: ('key, 'value, 'id ) t -> ('key * 'value) array 
val keysToArray: ('key, _, _) t -> 'key array    
val valuesToArray: (_,'value,_) t -> 'value array    
val ofArray: ('key * 'value) array -> dict:('key,'id) dict -> ('key, 'value, 'id ) t    
val mergeMany: ('key, 'value, 'id ) t -> ('key * 'value) array -> unit
val getBucketHistogram: _ t -> int array
val logStats: _ t -> unit
val logStats : _ t -> unit

