(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Hash tables and hash functions.

   Hash tables are hashed association tables, with in-place modification.
*)


(** {1 Generic interface} *)


type ('a, 'b) t
(** The type of hash tables from type ['a] to type ['b]. *)

val create : ?random:bool -> int -> ('a, 'b) t
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

val clear : ('a, 'b) t -> unit
(** Empty a hash table. Use [reset] instead of [clear] to shrink the
    size of the bucket table to its initial size. *)

val reset : ('a, 'b) t -> unit
(** Empty a hash table and shrink the size of the bucket table
    to its initial size.
    @since 4.00.0 *)

val copy : ('a, 'b) t -> ('a, 'b) t
(** Return a copy of the given hashtable. *)

val add : ('a, 'b) t -> 'a -> 'b -> unit
(** [Hashtbl.add tbl x y] adds a binding of [x] to [y] in table [tbl].
   Previous bindings for [x] are not removed, but simply
   hidden. That is, after performing {!Hashtbl.remove}[ tbl x],
   the previous binding for [x], if any, is restored.
   (Same behavior as with association lists.) *)

val find : ('a, 'b) t -> 'a -> 'b
(** [Hashtbl.find tbl x] returns the current binding of [x] in [tbl],
   or raises [Not_found] if no such binding exists. *)

val find_opt : ('a, 'b) t -> 'a -> 'b option
(** [Hashtbl.find_opt tbl x] returns the current binding of [x] in [tbl],
    or [None] if no such binding exists.
    @since 4.05 *)

val find_all : ('a, 'b) t -> 'a -> 'b list
(** [Hashtbl.find_all tbl x] returns the list of all data
   associated with [x] in [tbl].
   The current binding is returned first, then the previous
   bindings, in reverse order of introduction in the table. *)

val mem : ('a, 'b) t -> 'a -> bool
(** [Hashtbl.mem tbl x] checks if [x] is bound in [tbl]. *)

val remove : ('a, 'b) t -> 'a -> unit
(** [Hashtbl.remove tbl x] removes the current binding of [x] in [tbl],
   restoring the previous binding if it exists.
   It does nothing if [x] is not bound in [tbl]. *)

val replace : ('a, 'b) t -> 'a -> 'b -> unit
(** [Hashtbl.replace tbl x y] replaces the current binding of [x]
   in [tbl] by a binding of [x] to [y].  If [x] is unbound in [tbl],
   a binding of [x] to [y] is added to [tbl].
   This is functionally equivalent to {!Hashtbl.remove}[ tbl x]
   followed by {!Hashtbl.add}[ tbl x y]. *)

val iter : ('a -> 'b -> unit) -> ('a, 'b) t -> unit
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
   is entirely random.

   The behavior is not defined if the hash table is modified
   by [f] during the iteration.
*)

val filter_map_inplace: ('a -> 'b -> 'b option) -> ('a, 'b) t -> unit
(** [Hashtbl.filter_map_inplace f tbl] applies [f] to all bindings in
    table [tbl] and update each binding depending on the result of
    [f].  If [f] returns [None], the binding is discarded.  If it
    returns [Some new_val], the binding is update to associate the key
    to [new_val].

    Other comments for {!Hashtbl.iter} apply as well.
    @since 4.03.0 *)

val fold : ('a -> 'b -> 'c -> 'c) -> ('a, 'b) t -> 'c -> 'c
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
   is entirely random.

   The behavior is not defined if the hash table is modified
   by [f] during the iteration.
*)

val length : ('a, 'b) t -> int
(** [Hashtbl.length tbl] returns the number of bindings in [tbl].
   It takes constant time.  Multiple bindings are counted once each, so
   [Hashtbl.length] gives the number of times [Hashtbl.iter] calls its
   first argument. *)

val randomize : unit -> unit
(** After a call to [Hashtbl.randomize()], hash tables are created in
    randomized mode by default: {!Hashtbl.create} returns randomized
    hash tables, unless the [~random:false] optional parameter is given.
    The same effect can be achieved by setting the [R] parameter in
    the [OCAMLRUNPARAM] environment variable.

    It is recommended that applications or Web frameworks that need to
    protect themselves against the denial-of-service attack described
    in {!Hashtbl.create} call [Hashtbl.randomize()] at initialization
    time.

    Note that once [Hashtbl.randomize()] was called, there is no way
    to revert to the non-randomized default behavior of {!Hashtbl.create}.
    This is intentional.  Non-randomized hash tables can still be
    created using [Hashtbl.create ~random:false].

    @since 4.00.0 *)

val is_randomized : unit -> bool
(** return if the tables are currently created in randomized mode by default

    @since 4.03.0 *)

(** @since 4.00.0 *)
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
}

val stats : ('a, 'b) t -> statistics
(** [Hashtbl.stats tbl] returns statistics about the table [tbl]:
   number of buckets, size of the biggest bucket, distribution of
   buckets by size.
   @since 4.00.0 *)

(** {1 Functorial interface} *)

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
      IntHashtbl.add h 12 "hello"
    ]}

    This creates a new module [IntHashtbl], with a new type ['a
    IntHashtbl.t] of tables from [int] to ['a]. In this example, [h]
    contains [string] values so its type is [string IntHashtbl.t].

    Note that the new type ['a IntHashtbl.t] is not compatible with
    the type [('a,'b) Hashtbl.t] of the generic interface. For
    example, [Hashtbl.length h] would not type-check, you must use
    [IntHashtbl.length].
*)

module type HashedType =
  sig
    type t
    (** The type of the hashtable keys. *)

    val equal : t -> t -> bool
    (** The equality predicate used to compare keys. *)

    val hash : t -> int
      (** A hashing function on keys. It must be such that if two keys are
          equal according to [equal], then they have identical hash values
          as computed by [hash].
          Examples: suitable ([equal], [hash]) pairs for arbitrary key
          types include
-         ([(=)], {!Hashtbl.hash}) for comparing objects by structure
              (provided objects do not contain floats)
-         ([(fun x y -> compare x y = 0)], {!Hashtbl.hash})
              for comparing objects by structure
              and handling {!Pervasives.nan} correctly
-         ([(==)], {!Hashtbl.hash}) for comparing objects by physical
              equality (e.g. for mutable or cyclic objects). *)
   end
(** The input signature of the functor {!Hashtbl.Make}. *)

module type S =
  sig
    type key
    type 'a t
    val create : int -> 'a t
    val clear : 'a t -> unit
    val reset : 'a t -> unit (** @since 4.00.0 *)

    val copy : 'a t -> 'a t
    val add : 'a t -> key -> 'a -> unit
    val remove : 'a t -> key -> unit
    val find : 'a t -> key -> 'a
    val find_opt : 'a t -> key -> 'a option
    (** @since 4.05.0 *)

    val find_all : 'a t -> key -> 'a list
    val replace : 'a t -> key -> 'a -> unit
    val mem : 'a t -> key -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val filter_map_inplace: (key -> 'a -> 'a option) -> 'a t -> unit
    (** @since 4.03.0 *)

    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val length : 'a t -> int
    val stats: 'a t -> statistics (** @since 4.00.0 *)
  end
(** The output signature of the functor {!Hashtbl.Make}. *)

module Make (H : HashedType) : S with type key = H.t
(** Functor building an implementation of the hashtable structure.
    The functor [Hashtbl.Make] returns a structure containing
    a type [key] of keys and a type ['a t] of hash tables
    associating data of type ['a] to keys of type [key].
    The operations perform similarly to those of the generic
    interface, but use the hashing and equality functions
    specified in the functor argument [H] instead of generic
    equality and hashing.  Since the hash function is not seeded,
    the [create] operation of the result structure always returns
    non-randomized hash tables. *)

module type SeededHashedType =
  sig
    type t
    (** The type of the hashtable keys. *)

    val equal: t -> t -> bool
    (** The equality predicate used to compare keys. *)

    val hash: int -> t -> int
      (** A seeded hashing function on keys.  The first argument is
          the seed.  It must be the case that if [equal x y] is true,
          then [hash seed x = hash seed y] for any value of [seed].
          A suitable choice for [hash] is the function {!Hashtbl.seeded_hash}
          below. *)
  end
(** The input signature of the functor {!Hashtbl.MakeSeeded}.
    @since 4.00.0 *)

module type SeededS =
  sig
    type key
    type 'a t
    val create : ?random:bool -> int -> 'a t
    val clear : 'a t -> unit
    val reset : 'a t -> unit
    val copy : 'a t -> 'a t
    val add : 'a t -> key -> 'a -> unit
    val remove : 'a t -> key -> unit
    val find : 'a t -> key -> 'a
    val find_opt : 'a t -> key -> 'a option (** @since 4.05.0 *)

    val find_all : 'a t -> key -> 'a list
    val replace : 'a t -> key -> 'a -> unit
    val mem : 'a t -> key -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val filter_map_inplace: (key -> 'a -> 'a option) -> 'a t -> unit
    (** @since 4.03.0 *)

    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val length : 'a t -> int
    val stats: 'a t -> statistics
  end
(** The output signature of the functor {!Hashtbl.MakeSeeded}.
    @since 4.00.0 *)

module MakeSeeded (H : SeededHashedType) : SeededS with type key = H.t
(** Functor building an implementation of the hashtable structure.
    The functor [Hashtbl.MakeSeeded] returns a structure containing
    a type [key] of keys and a type ['a t] of hash tables
    associating data of type ['a] to keys of type [key].
    The operations perform similarly to those of the generic
    interface, but use the seeded hashing and equality functions
    specified in the functor argument [H] instead of generic
    equality and hashing.  The [create] operation of the
    result structure supports the [~random] optional parameter
    and returns randomized hash tables if [~random:true] is passed
    or if randomization is globally on (see {!Hashtbl.randomize}).
    @since 4.00.0 *)


(** {1 The polymorphic hash functions} *)


val hash : 'a -> int
(** [Hashtbl.hash x] associates a nonnegative integer to any value of
   any type. It is guaranteed that
   if [x = y] or [Pervasives.compare x y = 0], then [hash x = hash y].
   Moreover, [hash] always terminates, even on cyclic structures. *)

val seeded_hash : int -> 'a -> int
(** A variant of {!Hashtbl.hash} that is further parameterized by
   an integer seed.
   @since 4.00.0 *)

val hash_param : int -> int -> 'a -> int
(** [Hashtbl.hash_param meaningful total x] computes a hash value for [x],
   with the same properties as for [hash]. The two extra integer
   parameters [meaningful] and [total] give more precise control over
   hashing. Hashing performs a breadth-first, left-to-right traversal
   of the structure [x], stopping after [meaningful] meaningful nodes
   were encountered, or [total] nodes (meaningful or not) were
   encountered.  If [total] as specified by the user exceeds a certain
   value, currently 256, then it is capped to that value.
   Meaningful nodes are: integers; floating-point
   numbers; strings; characters; booleans; and constant
   constructors. Larger values of [meaningful] and [total] means that
   more nodes are taken into account to compute the final hash value,
   and therefore collisions are less likely to happen.  However,
   hashing takes longer. The parameters [meaningful] and [total]
   govern the tradeoff between accuracy and speed.  As default
   choices, {!Hashtbl.hash} and {!Hashtbl.seeded_hash} take
   [meaningful = 10] and [total = 100]. *)

val seeded_hash_param : int -> int -> int -> 'a -> int
(** A variant of {!Hashtbl.hash_param} that is further parameterized by
   an integer seed.  Usage:
   [Hashtbl.seeded_hash_param meaningful total seed x].
   @since 4.00.0 *)
