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

(** List operations.

   Some functions are flagged as not tail-recursive.  A tail-recursive
   function uses constant stack space, while a non-tail-recursive function
   uses stack space proportional to the length of its list argument, which
   can be a problem with very long lists.  When the function takes several
   list arguments, an approximate formula giving stack usage (in some
   unspecified constant unit) is shown in parentheses.

   The above considerations can usually be ignored if your lists are not
   longer than about 10000 elements.
*)

val length : 'a list -> int (* [@@dead "length"] *)
(** Return the length (number of elements) of the given list. *)

val hd : 'a list -> 'a (* [@@dead "hd"] *)
(** Return the first element of the given list. Raise
   [Failure "hd"] if the list is empty. *)

val compare_lengths : 'a list -> 'b list -> int (* [@@dead "compare_lengths"] *)
(** Compare the lengths of two lists. [compare_lengths l1 l2] is
   equivalent to [compare (length l1) (length l2)], except that
   the computation stops after itering on the shortest list.
   @since 4.05.0
 *)

val compare_length_with : 'a list -> len:int -> int (* [@@dead "compare_length_with"] *)
(** Compare the length of a list to an integer. [compare_length_with l n] is
   equivalent to [compare (length l) n], except that
   the computation stops after at most [n] iterations on the list.
   @since 4.05.0
*)

val cons : 'a -> 'a list -> 'a list (* [@@dead "cons"] *)
(** [cons x xs] is [x :: xs]
    @since 4.05.0
*)

val tl : 'a list -> 'a list (* [@@dead "tl"] *)
(** Return the given list without its first element. Raise
   [Failure "tl"] if the list is empty. *)

val nth : 'a list -> int -> 'a (* [@@dead "nth"] *)
(** Return the [n]-th element of the given list.
   The first element (head of the list) is at position 0.
   Raise [Failure "nth"] if the list is too short.
   Raise [Invalid_argument "List.nth"] if [n] is negative. *)

val nth_opt: 'a list -> int -> 'a option (* [@@dead "nth_opt"] *)
(** Return the [n]-th element of the given list.
    The first element (head of the list) is at position 0.
    Return [None] if the list is too short.
    Raise [Invalid_argument "List.nth"] if [n] is negative.
    @since 4.05
*)

val rev : 'a list -> 'a list (* [@@dead "rev"] *)
(** List reversal. *)

val init : len:int -> f:(int -> 'a) -> 'a list (* [@@dead "init"] *)
(** [List.init len f] is [f 0; f 1; ...; f (len-1)], evaluated left to right.

    @raise Invalid_argument if [len < 0].
    @since 4.06.0
*)

val append : 'a list -> 'a list -> 'a list (* [@@dead "append"] *)
(** Catenate two lists.  Same function as the infix operator [@].
   Not tail-recursive (length of the first argument).  The [@]
   operator is not tail-recursive either. *)

val rev_append : 'a list -> 'a list -> 'a list (* [@@dead "rev_append"] *)
(** [List.rev_append l1 l2] reverses [l1] and concatenates it with [l2].
   This is equivalent to [(]{!List.rev}[ l1) @ l2], but [rev_append] is
   tail-recursive and more efficient. *)

val concat : 'a list list -> 'a list (* [@@dead "concat"] *)
(** Concatenate a list of lists.  The elements of the argument are all
   concatenated together (in the same order) to give the result.
   Not tail-recursive
   (length of the argument + length of the longest sub-list). *)

val flatten : 'a list list -> 'a list (* [@@dead "flatten"] *)
(** Same as [concat].  Not tail-recursive
   (length of the argument + length of the longest sub-list). *)


(** {1 Iterators} *)


val iter : f:('a -> unit) -> 'a list -> unit (* [@@dead "iter"] *)
(** [List.iter f [a1; ...; an]] applies function [f] in turn to
   [a1; ...; an]. It is equivalent to
   [begin f a1; f a2; ...; f an; () end]. *)

val iteri : f:(int -> 'a -> unit) -> 'a list -> unit (* [@@dead "iteri"] *)
(** Same as {!List.iter}, but the function is applied to the index of
   the element as first argument (counting from 0), and the element
   itself as second argument.
   @since 4.00.0
*)

val map : f:('a -> 'b) -> 'a list -> 'b list (* [@@dead "map"] *)
(** [List.map f [a1; ...; an]] applies function [f] to [a1, ..., an],
   and builds the list [[f a1; ...; f an]]
   with the results returned by [f].  Not tail-recursive. *)

val mapi : f:(int -> 'a -> 'b) -> 'a list -> 'b list (* [@@dead "mapi"] *)
(** Same as {!List.map}, but the function is applied to the index of
   the element as first argument (counting from 0), and the element
   itself as second argument.
   @since 4.00.0
*)

val rev_map : f:('a -> 'b) -> 'a list -> 'b list (* [@@dead "rev_map"] *)
(** [List.rev_map f l] gives the same result as
   {!List.rev}[ (]{!List.map}[ f l)], but is tail-recursive and
   more efficient. *)

val fold_left : f:('a -> 'b -> 'a) -> init:'a -> 'b list -> 'a (* [@@dead "fold_left"] *)
(** [List.fold_left f a [b1; ...; bn]] is
   [f (... (f (f a b1) b2) ...) bn]. *)

val fold_right : f:('a -> 'b -> 'b) -> 'a list -> init:'b -> 'b (* [@@dead "fold_right"] *)
(** [List.fold_right f [a1; ...; an] b] is
   [f a1 (f a2 (... (f an b) ...))].  Not tail-recursive. *)


(** {1 Iterators on two lists} *)


val iter2 : f:('a -> 'b -> unit) -> 'a list -> 'b list -> unit (* [@@dead "iter2"] *)
(** [List.iter2 f [a1; ...; an] [b1; ...; bn]] calls in turn
   [f a1 b1; ...; f an bn].
   Raise [Invalid_argument] if the two lists are determined
   to have different lengths. *)

val map2 : f:('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list (* [@@dead "map2"] *)
(** [List.map2 f [a1; ...; an] [b1; ...; bn]] is
   [[f a1 b1; ...; f an bn]].
   Raise [Invalid_argument] if the two lists are determined
   to have different lengths.  Not tail-recursive. *)

val rev_map2 : f:('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list (* [@@dead "rev_map2"] *)
(** [List.rev_map2 f l1 l2] gives the same result as
   {!List.rev}[ (]{!List.map2}[ f l1 l2)], but is tail-recursive and
   more efficient. *)

val fold_left2 : (* [@@dead "fold_left2"] *)
  f:('a -> 'b -> 'c -> 'a) -> init:'a -> 'b list -> 'c list -> 'a
(** [List.fold_left2 f a [b1; ...; bn] [c1; ...; cn]] is
   [f (... (f (f a b1 c1) b2 c2) ...) bn cn].
   Raise [Invalid_argument] if the two lists are determined
   to have different lengths. *)

val fold_right2 : (* [@@dead "fold_right2"] *)
  f:('a -> 'b -> 'c -> 'c) -> 'a list -> 'b list -> init:'c -> 'c
(** [List.fold_right2 f [a1; ...; an] [b1; ...; bn] c] is
   [f a1 b1 (f a2 b2 (... (f an bn c) ...))].
   Raise [Invalid_argument] if the two lists are determined
   to have different lengths.  Not tail-recursive. *)


(** {1 List scanning} *)


val for_all : f:('a -> bool) -> 'a list -> bool (* [@@dead "for_all"] *)
(** [for_all p [a1; ...; an]] checks if all elements of the list
   satisfy the predicate [p]. That is, it returns
   [(p a1) && (p a2) && ... && (p an)]. *)

val exists : f:('a -> bool) -> 'a list -> bool (* [@@dead "exists"] *)
(** [exists p [a1; ...; an]] checks if at least one element of
   the list satisfies the predicate [p]. That is, it returns
   [(p a1) || (p a2) || ... || (p an)]. *)

val for_all2 : f:('a -> 'b -> bool) -> 'a list -> 'b list -> bool (* [@@dead "for_all2"] *)
(** Same as {!List.for_all}, but for a two-argument predicate.
   Raise [Invalid_argument] if the two lists are determined
   to have different lengths. *)

val exists2 : f:('a -> 'b -> bool) -> 'a list -> 'b list -> bool (* [@@dead "exists2"] *)
(** Same as {!List.exists}, but for a two-argument predicate.
   Raise [Invalid_argument] if the two lists are determined
   to have different lengths. *)

val mem : 'a -> set:'a list -> bool (* [@@dead "mem"] *)
(** [mem a l] is true if and only if [a] is equal
   to an element of [l]. *)

val memq : 'a -> set:'a list -> bool (* [@@dead "memq"] *)
(** Same as {!List.mem}, but uses physical equality instead of structural
   equality to compare list elements. *)


(** {1 List searching} *)


val find : f:('a -> bool) -> 'a list -> 'a (* [@@dead "find"] *)
(** [find p l] returns the first element of the list [l]
   that satisfies the predicate [p].
   Raise [Not_found] if there is no value that satisfies [p] in the
   list [l]. *)

val find_opt: f:('a -> bool) -> 'a list -> 'a option (* [@@dead "find_opt"] *)
(** [find p l] returns the first element of the list [l]
   that satisfies the predicate [p].
   Returns [None] if there is no value that satisfies [p] in the
   list [l].
   @since 4.05 *)

val filter : f:('a -> bool) -> 'a list -> 'a list (* [@@dead "filter"] *)
(** [filter p l] returns all the elements of the list [l]
   that satisfy the predicate [p].  The order of the elements
   in the input list is preserved.  *)

val find_all : f:('a -> bool) -> 'a list -> 'a list (* [@@dead "find_all"] *)
(** [find_all] is another name for {!List.filter}. *)

val partition : f:('a -> bool) -> 'a list -> 'a list * 'a list (* [@@dead "partition"] *)
(** [partition p l] returns a pair of lists [(l1, l2)], where
   [l1] is the list of all the elements of [l] that
   satisfy the predicate [p], and [l2] is the list of all the
   elements of [l] that do not satisfy [p].
   The order of the elements in the input list is preserved. *)


(** {1 Association lists} *)


val assoc : 'a -> ('a * 'b) list -> 'b (* [@@dead "assoc"] *)
(** [assoc a l] returns the value associated with key [a] in the list of
   pairs [l]. That is,
   [assoc a [ ...; (a,b); ...] = b]
   if [(a,b)] is the leftmost binding of [a] in list [l].
   Raise [Not_found] if there is no value associated with [a] in the
   list [l]. *)

val assoc_opt: 'a -> ('a * 'b) list -> 'b option (* [@@dead "assoc_opt"] *)
(** [assoc_opt a l] returns the value associated with key [a] in the list of
    pairs [l]. That is,
    [assoc a [ ...; (a,b); ...] = b]
    if [(a,b)] is the leftmost binding of [a] in list [l].
    Returns [None] if there is no value associated with [a] in the
    list [l].
    @since 4.05
*)

val assq : 'a -> ('a * 'b) list -> 'b (* [@@dead "assq"] *)
(** Same as {!List.assoc}, but uses physical equality instead of
   structural equality to compare keys. *)

val assq_opt: 'a -> ('a * 'b) list -> 'b option (* [@@dead "assq_opt"] *)
(** Same as {!List.assoc_opt}, but uses physical equality instead of
   structural equality to compare keys.
   @since 4.05.0 *)

val mem_assoc : 'a -> map:('a * 'b) list -> bool (* [@@dead "mem_assoc"] *)
(** Same as {!List.assoc}, but simply return true if a binding exists,
   and false if no bindings exist for the given key. *)

val mem_assq : 'a -> map:('a * 'b) list -> bool (* [@@dead "mem_assq"] *)
(** Same as {!List.mem_assoc}, but uses physical equality instead of
   structural equality to compare keys. *)

val remove_assoc : 'a -> ('a * 'b) list -> ('a * 'b) list (* [@@dead "remove_assoc"] *)
(** [remove_assoc a l] returns the list of
   pairs [l] without the first pair with key [a], if any.
   Not tail-recursive. *)

val remove_assq : 'a -> ('a * 'b) list -> ('a * 'b) list (* [@@dead "remove_assq"] *)
(** Same as {!List.remove_assoc}, but uses physical equality instead
   of structural equality to compare keys.  Not tail-recursive. *)


(** {1 Lists of pairs} *)


val split : ('a * 'b) list -> 'a list * 'b list (* [@@dead "split"] *)
(** Transform a list of pairs into a pair of lists:
   [split [(a1,b1); ...; (an,bn)]] is [([a1; ...; an], [b1; ...; bn])].
   Not tail-recursive.
*)

val combine : 'a list -> 'b list -> ('a * 'b) list (* [@@dead "combine"] *)
(** Transform a pair of lists into a list of pairs:
   [combine [a1; ...; an] [b1; ...; bn]] is
   [[(a1,b1); ...; (an,bn)]].
   Raise [Invalid_argument] if the two lists
   have different lengths.  Not tail-recursive. *)


(** {1 Sorting} *)


val sort : cmp:('a -> 'a -> int) -> 'a list -> 'a list (* [@@dead "sort"] *)
(** Sort a list in increasing order according to a comparison
   function.  The comparison function must return 0 if its arguments
   compare as equal, a positive integer if the first is greater,
   and a negative integer if the first is smaller (see Array.sort for
   a complete specification).  For example,
   {!Pervasives.compare} is a suitable comparison function.
   The resulting list is sorted in increasing order.
   [List.sort] is guaranteed to run in constant heap space
   (in addition to the size of the result list) and logarithmic
   stack space.

   The current implementation uses Merge Sort. It runs in constant
   heap space and logarithmic stack space.
*)

val stable_sort : cmp:('a -> 'a -> int) -> 'a list -> 'a list (* [@@dead "stable_sort"] *)
(** Same as {!List.sort}, but the sorting algorithm is guaranteed to
   be stable (i.e. elements that compare equal are kept in their
   original order) .

   The current implementation uses Merge Sort. It runs in constant
   heap space and logarithmic stack space.
*)

val fast_sort : cmp:('a -> 'a -> int) -> 'a list -> 'a list (* [@@dead "fast_sort"] *)
(** Same as {!List.sort} or {!List.stable_sort}, whichever is
    faster on typical input. *)

val sort_uniq : cmp:('a -> 'a -> int) -> 'a list -> 'a list (* [@@dead "sort_uniq"] *)
(** Same as {!List.sort}, but also remove duplicates.
    @since 4.03.0 *)

val merge : cmp:('a -> 'a -> int) -> 'a list -> 'a list -> 'a list (* [@@dead "merge"] *)
(** Merge two lists:
    Assuming that [l1] and [l2] are sorted according to the
    comparison function [cmp], [merge cmp l1 l2] will return a
    sorted list containing all the elements of [l1] and [l2].
    If several elements compare equal, the elements of [l1] will be
    before the elements of [l2].
    Not tail-recursive (sum of the lengths of the arguments).
*)