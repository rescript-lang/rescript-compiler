



type ('a,'b,'id) t 
  
(** The type of hash tables from type ['a] to type ['b]. *)

type ('a, 'id) dict = ('a, 'id) Bs_Dict.hashable
    
val make :  int -> dict: ('a, 'id) dict ->  ('a,'b,'id) t
(** [make n ~dict] creates a new, empty hash table, with
    initial size [n].  For best results, [n] should be on the
    order of the expected number of elements that will be in
    the table.  The table grows as needed, so [n] is just an
    initial guess.
*)


val clear : ('a, 'b, 'id) t -> unit
(** Empty a hash table. Use [reset] instead of [clear] to shrink the
    size of the bucket table to its initial size. *)





val add: ('a, 'b, 'id) t -> 'a -> 'b -> unit
(** [add tbl x y] adds a binding of [x] to [y] in table [tbl].
    Previous bindings for [x] are not removed, but simply
    hidden. That is, after performing [remove tbl x],
    the previous binding for [x], if any, is restored.
    (Same behavior as with association lists.) *)

val get:  
  ('a, 'b, 'id) t -> 'a -> 'b option
(** [get tbl x] returns the current binding of [x] in [tbl] *)

val getAll:  ('a, 'b, 'id) t -> 'a -> 'b list  
(** [getAll tbl x] returns the list of all data
    associated with [x] in [tbl].
    The current binding is returned first, then the previous
    bindings, in reverse order of introduction in the table. *)

val has:  
  ('a, 'b, 'id) t -> 'a -> bool
(** [has tbl x] checks if [x] is bound in [tbl]. *)

  
val remove:
('a, 'b, 'id) t -> 'a -> unit
(** [remove tbl x] removes the current binding of [x] in [tbl],
    restoring the previous binding if it exists.
    It does nothing if [x] is not bound in [tbl]. *)

val removeAll:
('a, 'b, 'id) t -> 'a -> unit

    
val replace:  
  ('a, 'b, 'id) t -> 'a -> 'b -> unit
(** [replace tbl x y] replaces the current binding of [x]
    in [tbl] by a binding of [x] to [y].  If [x] is unbound in [tbl],
    a binding of [x] to [y] is added to [tbl].
    This is functionally equivalent to [remove tbl x]
    followed by [add tbl x y]. *)


val forEach : ('a, 'b, 'id) t -> ('a -> 'b -> unit [@bs]) -> unit
(** [forEach f tbl] applies [f] to all bindings in table [tbl].
    [f] receives the key as first argument, and the associated value
    as second argument. Each binding is presented exactly once to [f].

    The order in which the bindings are passed to [f] is unspecified.
    However, if the table contains several bindings for the same key,
    they are passed to [f] in reverse order of introduction, that is,
    the most recent binding is passed first.
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

    If the hash table was created in non-randomized mode, the order
    in which the bindings are enumerated is reproducible between
    successive runs of the program, and even between minor versions
    of OCaml.  For randomized hash tables, the order of enumeration
    is entirely random. *)


val keepMapInPlace : ('a, 'b, 'id) t -> ('a -> 'b -> 'b option [@bs]) ->  unit
  
val size  : ('a, 'b, 'id) t -> int  
(** [size tbl] returns the number of bindings in [tbl].
    It takes constant time.  Multiple bindings are counted once each, so
    [size] gives the number of times [forEach] calls its
    first argument. *)




val logStats : _ t -> unit

