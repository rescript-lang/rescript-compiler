# 4 "hashmap.cppo.mli"
type key = int


# 10
type 'b t 


val create :  int -> 'b t 
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


val clear : 'b t -> unit
(** Empty a hash table. Use [reset] instead of [clear] to shrink the
    size of the bucket table to its initial size. *)


val reset : 'b t -> unit
(** Empty a hash table and shrink the size of the bucket table
    to its initial size.
    @since 4.00.0 *)




val add : 'b t -> key -> 'b -> unit
(** [Hashtbl.add tbl x y] adds a binding of [x] to [y] in table [tbl].
    Previous bindings for [x] are not removed, but simply
    hidden. That is, after performing {!Hashtbl.remove}[ tbl x],
    the previous binding for [x], if any, is restored.
    (Same behavior as with association lists.) *)

val findOpt:  
   'b t -> key -> 'b option
(** [findOpt tbl x] returns the current binding of [x] in [tbl],
    *)

val findAll:  'b t -> key -> 'b list  
(** [Hashtbl.find_all tbl x] returns the list of all data
    associated with [x] in [tbl].
    The current binding is returned first, then the previous
    bindings, in reverse order of introduction in the table. *)

val mem:  
  'b  t -> key -> bool
(** [Hashtbl.mem tbl x] checks if [x] is bound in [tbl]. *)

val remove:
  'b t -> key -> unit
(** [Hashtbl.remove tbl x] removes the current binding of [x] in [tbl],
    restoring the previous binding if it exists.
    It does nothing if [x] is not bound in [tbl]. *)

val removeAll:
  'b t -> key -> unit

    
val replace:  
  'b t -> key -> 'b -> unit
(** [Hashtbl.replace tbl x y] replaces the current binding of [x]
    in [tbl] by a binding of [x] to [y].  If [x] is unbound in [tbl],
    a binding of [x] to [y] is added to [tbl].
    This is functionally equivalent to {!Hashtbl.remove}[ tbl x]
    followed by {!Hashtbl.add}[ tbl x y]. *)


val iter : (key -> 'b -> unit [@bs]) -> 'b t -> unit
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


val fold : (key -> 'b -> 'c -> 'c [@bs]) -> 'b t -> 'c -> 'c
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


val filterMapInplace : (key -> 'b -> 'b option [@bs]) -> 'b t -> unit
  
val length  : 'b t -> int  
(** [Hashtbl.length tbl] returns the number of bindings in [tbl].
    It takes constant time.  Multiple bindings are counted once each, so
    [Hashtbl.length] gives the number of times [Hashtbl.iter] calls its
    first argument. *)


(* 
type statistics = {
  num_bindings: int;
  (** Number of bindings present in the table.
      Same value as returned by {!Hashtbl.length}. *)
  num_buckets: int;
  (** Number of buckets in the table. *)
  max_bucket_length: int;
  (** Maximal number of bindings per bucket. *)
  bucket_histogram: int array
  (** Histogram of bucket sizes.  This array [histo] has
      length [max_bucket_length + 1].  The value of
      [histo.(i)] is the number of buckets whose size is [i]. *)
} *)

val logStats : _ t -> unit
(** [Hashtbl.stats tbl] returns statistics about the table [tbl]:
    number of buckets, size of the biggest bucket, distribution of
    buckets by size.
    @since 4.00.0 *)

(** {6 Functorial interface} *)

(** The functorial interface allows the use of specific comparison
    and hash functions, either for performance/security concerns,
    or because keys are not hashable/comparable with the polymorphic builtins.

    For instance, one might want to specialize a table for integer keys:
    {[
      module IntHash =
      struct
        type t = int
        let equal i j = i=j
        let hash i = i land max_int
      end

      module IntHashtbl = Hashtbl.Make(IntHash)

      let h = IntHashtbl.create 17 in
      IntHashtbl.add h 12 "hello";;
    ]}

    This creates a new module [IntHashtbl], with a new type ['a
    IntHashtbl.t] of tables from [int] to ['a]. In this example, [h]
    contains [string] values so its type is [string IntHashtbl.t].

    Note that the new type ['a IntHashtbl.t] is not compatible with
    the type [('a,'b) Hashtbl.t] of the generic interface. For
    example, [Hashtbl.length h] would not type-check, you must use
    [IntHashtbl.length].
*)



