

type ('a, 'b, 'id) t0

type ('a,'b,'id) t = 
    (('a, 'id) Bs_Hash.t,
     ('a,'b,'id) t0) Bs_Bag.bag 
  
(** The type of hash tables from type ['a] to type ['b]. *)

val create0 : int -> ('a, 'b, 'id) t0
val create : ('a,'id) Bs_Hash.t -> int -> ('a,'b,'id) t
(** [Hashtbl.create n] creates a new, empty hash table, with
    initial size [n].  For best results, [n] should be on the
    order of the expected number of elements that will be in
    the table.  The table grows as needed, so [n] is just an
    initial guess.

    The optional [random] parameter (a boolean) controls whether
    the internal organization of the hash table is randomized at each
    execution of [Hashtbl.create] or deterministic over all executions.

    A hash table that is created with [~random:false] uses a
    fixed hash function ({!Hashtbl.hash}) to distribute keys among
    buckets.  As a consequence, collisions between keys happen
    deterministically.  In Web-facing applications or other
    security-sensitive applications, the deterministic collision
    patterns can be exploited by a malicious user to create a
    denial-of-service attack: the attacker sends input crafted to
    create many collisions in the table, slowing the application down.

    A hash table that is created with [~random:true] uses the seeded
    hash function {!Hashtbl.seeded_hash} with a seed that is randomly
    chosen at hash table creation time.  In effect, the hash function
    used is randomly selected among [2^{30}] different hash functions.
    All these hash functions have different collision patterns,
    rendering ineffective the denial-of-service attack described above.
    However, because of randomization, enumerating all elements of the
    hash table using {!Hashtbl.fold} or {!Hashtbl.iter} is no longer
    deterministic: elements are enumerated in different orders at
    different runs of the program.

    If no [~random] parameter is given, hash tables are created
    in non-random mode by default.  This default can be changed
    either programmatically by calling {!Hashtbl.randomize} or by
    setting the [R] flag in the [OCAMLRUNPARAM] environment variable.

    @before 4.00.0 the [random] parameter was not present and all
    hash tables were created in non-randomized mode. *)

val clear0 : ('a, 'b, 'id) t0 -> unit
val clear : ('a, 'b, 'id) t -> unit
(** Empty a hash table. Use [reset] instead of [clear] to shrink the
    size of the bucket table to its initial size. *)

val reset0 : ('a, 'b, 'id) t0 -> unit
val reset : ('a, 'b, 'id) t -> unit
(** Empty a hash table and shrink the size of the bucket table
    to its initial size.
    @since 4.00.0 *)



val add0 : 
    hash:('a,'id) Bs_Hash.hash ->
    eq:('a,'id) Bs_Hash.eq -> 
     ('a,'b,'id) t0 -> 'a -> 'b -> unit
val add : ('a, 'b, 'id) t -> 'a -> 'b -> unit
(** [Hashtbl.add tbl x y] adds a binding of [x] to [y] in table [tbl].
    Previous bindings for [x] are not removed, but simply
    hidden. That is, after performing {!Hashtbl.remove}[ tbl x],
    the previous binding for [x], if any, is restored.
    (Same behavior as with association lists.) *)

val findOpt0: 
  hash:('a,'id) Bs_Hash.hash  -> 
  eq:('a,'id) Bs_Hash.eq -> 
  ('a, 'b, 'id) t0 -> 'a -> 'b option
val findOpt:  
  ('a, 'b, 'id) t -> 'a -> 'b option
(** [findOpt tbl x] returns the current binding of [x] in [tbl],
    *)


val mem0: 
  hash:('a,'id) Bs_Hash.hash  -> 
  eq:('a,'id) Bs_Hash.eq -> 
  ('a, 'b, 'id) t0 -> 'a -> bool
val mem:  
  ('a, 'b, 'id) t -> 'a -> bool
(** [Hashtbl.mem tbl x] checks if [x] is bound in [tbl]. *)

val remove0: 
  hash:('a,'id) Bs_Hash.hash  -> 
  eq:('a,'id) Bs_Hash.eq -> 
  ('a, 'b, 'id) t0 -> 'a -> unit
val remove:
('a, 'b, 'id) t -> 'a -> unit
(** [Hashtbl.remove tbl x] removes the current binding of [x] in [tbl],
    restoring the previous binding if it exists.
    It does nothing if [x] is not bound in [tbl]. *)



    


val iter0 : ('a, 'b, 'id) t0 -> ('a -> 'b -> unit [@bs]) -> unit
val iter : ('a, 'b, 'id) t -> ('a -> 'b -> unit [@bs]) -> unit
(** [Hashtbl.iter f tbl] applies [f] to all bindings in table [tbl].
    [f] receives the key as first argument, and the associated value
    as second argument. Each binding is presented exactly once to [f].

    The order in which the bindings are passed to [f] is unspecified.
    However, if the table contains several bindings for the same key,
    they are passed to [f] in reverse order of introduction, that is,
    the most recent binding is passed first.

    If the hash table was created in non-randomized mode, the order
    in which the bindings are enumerated is reproducible between
    successive runs of the program, and even between minor versions
    of OCaml.  For randomized hash tables, the order of enumeration
    is entirely random. *)

val fold0 : ('a, 'b, 'id) t0 -> 'c -> ('a -> 'b -> 'c -> 'c [@bs]) -> 'c
val fold : ('a, 'b, 'id) t -> 'c -> ('a -> 'b -> 'c -> 'c [@bs]) ->  'c
(** [Hashtbl.fold f tbl init] computes
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

val filterMapInplace0 : ('a, 'b, 'id) t0 -> ('a -> 'b -> 'b option [@bs]) -> unit
val filterMapInplace : ('a, 'b, 'id) t -> ('a -> 'b -> 'b option [@bs]) ->  unit
  
val length0 : ('a, 'b, 'id) t0 -> int
val length  : ('a, 'b, 'id) t -> int  
(** [Hashtbl.length tbl] returns the number of bindings in [tbl].
    It takes constant time.  Multiple bindings are counted once each, so
    [Hashtbl.length] gives the number of times [Hashtbl.iter] calls its
    first argument. *)



val logStats0 : ('a, 'b, 'id) t0 -> unit
val logStats : _ t -> unit
(** [Hashtbl.stats tbl] returns statistics about the table [tbl]:
    number of buckets, size of the biggest bucket, distribution of
    buckets by size.
    @since 4.00.0 *)




val toArray0 : ('a, 'b, 'id) t0 -> ('a * 'b) array
val toArray : ('a, 'b, 'id) t -> ('a * 'b) array 


val ofArray0 : 
  hash:('a,'id) Bs_Hash.hash  -> 
  eq:('a,'id) Bs_Hash.eq -> 
  ('a * 'b) array -> 
  ('a, 'b, 'id) t0      

val ofArray :   
  dict:('a,'id) Bs_Hash.t -> 
  ('a * 'b) array -> 
  ('a, 'b, 'id) t 

val addArray0 : 
  hash:('a,'id) Bs_Hash.hash  -> 
  eq:('a,'id) Bs_Hash.eq -> 
  ('a, 'b, 'id) t0 -> ('a * 'b) array -> unit     
val addArray:   
   ('a, 'b, 'id) t -> ('a * 'b) array -> unit   

val keys0 :    
    ('a,'b,'id) t0 -> 'a array
val keys :    
    ('a,'b,'id) t -> 'a array    

val values0 :    
    ('a,'b,'id) t0 -> 'b array
val values :    
    ('a,'b,'id) t -> 'b array    
    