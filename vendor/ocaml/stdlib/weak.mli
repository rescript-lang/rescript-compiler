(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Damien Doligez, projet Para, INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 1997 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(** Arrays of weak pointers and hash tables of weak pointers. *)


(** {6 Low-level functions} *)

type 'a t
(** The type of arrays of weak pointers (weak arrays).  A weak
   pointer is a value that the garbage collector may erase whenever
   the value is not used any more (through normal pointers) by the
   program.  Note that finalisation functions are run after the
   weak pointers are erased.

   A weak pointer is said to be full if it points to a value,
   empty if the value was erased by the GC.

   Notes:
   - Integers are not allocated and cannot be stored in weak arrays.
   - Weak arrays cannot be marshaled using {!Pervasives.output_value}
     nor the functions of the {!Marshal} module.
*)


val create : int -> 'a t
(** [Weak.create n] returns a new weak array of length [n].
   All the pointers are initially empty.  Raise [Invalid_argument]
   if [n] is negative or greater than {!Sys.max_array_length}[-1].*)

val length : 'a t -> int
(** [Weak.length ar] returns the length (number of elements) of
   [ar].*)

val set : 'a t -> int -> 'a option -> unit
(** [Weak.set ar n (Some el)] sets the [n]th cell of [ar] to be a
   (full) pointer to [el]; [Weak.set ar n None] sets the [n]th
   cell of [ar] to empty.
   Raise [Invalid_argument "Weak.set"] if [n] is not in the range
   0 to {!Weak.length}[ a - 1].*)

val get : 'a t -> int -> 'a option
(** [Weak.get ar n] returns None if the [n]th cell of [ar] is
   empty, [Some x] (where [x] is the value) if it is full.
   Raise [Invalid_argument "Weak.get"] if [n] is not in the range
   0 to {!Weak.length}[ a - 1].*)

val get_copy : 'a t -> int -> 'a option
(** [Weak.get_copy ar n] returns None if the [n]th cell of [ar] is
   empty, [Some x] (where [x] is a (shallow) copy of the value) if
   it is full.
   In addition to pitfalls with mutable values, the interesting
   difference with [get] is that [get_copy] does not prevent
   the incremental GC from erasing the value in its current cycle
   ([get] may delay the erasure to the next GC cycle).
   Raise [Invalid_argument "Weak.get"] if [n] is not in the range
   0 to {!Weak.length}[ a - 1].*)


val check : 'a t -> int -> bool
(** [Weak.check ar n] returns [true] if the [n]th cell of [ar] is
   full, [false] if it is empty.  Note that even if [Weak.check ar n]
   returns [true], a subsequent {!Weak.get}[ ar n] can return [None].*)

val fill : 'a t -> int -> int -> 'a option -> unit
(** [Weak.fill ar ofs len el] sets to [el] all pointers of [ar] from
   [ofs] to [ofs + len - 1].  Raise [Invalid_argument "Weak.fill"]
   if [ofs] and [len] do not designate a valid subarray of [a].*)

val blit : 'a t -> int -> 'a t -> int -> int -> unit
(** [Weak.blit ar1 off1 ar2 off2 len] copies [len] weak pointers
   from [ar1] (starting at [off1]) to [ar2] (starting at [off2]).
   It works correctly even if [ar1] and [ar2] are the same.
   Raise [Invalid_argument "Weak.blit"] if [off1] and [len] do
   not designate a valid subarray of [ar1], or if [off2] and [len]
   do not designate a valid subarray of [ar2].*)


(** {6 Weak hash tables} *)

(** A weak hash table is a hashed set of values.  Each value may
    magically disappear from the set when it is not used by the
    rest of the program any more.  This is normally used to share
    data structures without inducing memory leaks.
    Weak hash tables are defined on values from a {!Hashtbl.HashedType}
    module; the [equal] relation and [hash] function are taken from that
    module.  We will say that [v] is an instance of [x] if [equal x v]
    is [true].

    The [equal] relation must be able to work on a shallow copy of
    the values and give the same result as with the values themselves.
    *)

module type S = sig
  type data
    (** The type of the elements stored in the table. *)
  type t
    (** The type of tables that contain elements of type [data].
        Note that weak hash tables cannot be marshaled using
        {!Pervasives.output_value} or the functions of the {!Marshal}
        module. *)
  val create : int -> t
    (** [create n] creates a new empty weak hash table, of initial
        size [n].  The table will grow as needed. *)
  val clear : t -> unit
    (** Remove all elements from the table. *)
  val merge : t -> data -> data
    (** [merge t x] returns an instance of [x] found in [t] if any,
        or else adds [x] to [t] and return [x]. *)
  val add : t -> data -> unit
    (** [add t x] adds [x] to [t].  If there is already an instance
        of [x] in [t], it is unspecified which one will be
        returned by subsequent calls to [find] and [merge]. *)
  val remove : t -> data -> unit
    (** [remove t x] removes from [t] one instance of [x].  Does
        nothing if there is no instance of [x] in [t]. *)
  val find : t -> data -> data
    (** [find t x] returns an instance of [x] found in [t].
        Raise [Not_found] if there is no such element. *)
  val find_all : t -> data -> data list
    (** [find_all t x] returns a list of all the instances of [x]
        found in [t]. *)
  val mem : t -> data -> bool
    (** [mem t x] returns [true] if there is at least one instance
        of [x] in [t], false otherwise. *)
  val iter : (data -> unit) -> t -> unit
    (** [iter f t] calls [f] on each element of [t], in some unspecified
        order.  It is not specified what happens if [f] tries to change
        [t] itself. *)
  val fold : (data -> 'a -> 'a) -> t -> 'a -> 'a
    (** [fold f t init] computes [(f d1 (... (f dN init)))] where
        [d1 ... dN] are the elements of [t] in some unspecified order.
        It is not specified what happens if [f] tries to change [t]
        itself. *)
  val count : t -> int
    (** Count the number of elements in the table.  [count t] gives the
        same result as [fold (fun _ n -> n+1) t 0] but does not delay the
        deallocation of the dead elements. *)
  val stats : t -> int * int * int * int * int * int
    (** Return statistics on the table.  The numbers are, in order:
        table length, number of entries, sum of bucket lengths,
        smallest bucket length, median bucket length, biggest bucket length. *)
end;;
(** The output signature of the functor {!Weak.Make}. *)

module Make (H : Hashtbl.HashedType) : S with type data = H.t;;
(** Functor building an implementation of the weak hash table structure. *)
