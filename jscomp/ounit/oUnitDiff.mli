(***********************************************************************)
(* The OUnit library                                                   *)
(*                                                                     *)
(* Copyright (C) 2010 OCamlCore SARL                                   *)
(*                                                                     *)
(* See LICENSE for details.                                            *)
(***********************************************************************)

(** Unit tests for collection of elements
  
    This module allows to define a more precise way to display differences
    between collection of elements. When collection differ, the tester is 
    interested by what are the missing/extra elements. This module provides
    a [diff] operation to spot the difference quickly between two sets of
    elements.

    Example:
{[
open OUnit;;

module EInt = 
struct 
  type t = int
  let compare = ( - )
  let pp_printer = Format.pp_print_int
  let pp_print_sep = OUnitDiff.pp_comma_separator
end

module ListInt = OUnitDiff.ListSimpleMake(EInt);;

let test_diff () = 
  ListInt.assert_equal
    [1; 2; 3; 4; 5]
    [1; 2; 5; 4]
;;

let _ = 
  run_test_tt_main ("test_diff" >:: test_diff)
;;
]}

when run this test outputs:
{[
OUnit: expected: 1, 2, 3, 4, 5 but got: 1, 2, 5, 4
differences: element number 2 differ (3 <> 5)
]}

  @since 1.1.0
  @author Sylvain Le Gall
  *)

(** {2 Signatures} *)

(** Definition of an element 
  *)
module type DIFF_ELEMENT =
  sig
    (** Type of an element *)
    type t

    (** Pretty printer for an element *)
    val pp_printer : Format.formatter -> t -> unit

    (** Element comparison *)
    val compare : t -> t -> int

    (** Pretty print element separator *)
    val pp_print_sep : Format.formatter -> unit -> unit
  end

(** Definition of standard operations
  *)
module type S =
  sig
    (** Type of an element *)
    type e 

    (** Type of a collection of element *)
    type t

    (** Compare a collection of element *)
    val compare : t -> t -> int

    (** Pretty printer a collection of element *)
    val pp_printer : Format.formatter -> t -> unit

    (** Pretty printer for collection differences *)
    val pp_diff : Format.formatter -> t * t -> unit

    (** {!assert_equal} with [~diff], [~cmp] and [~printer] predefined for
        this collection events
      *)
    val assert_equal : ?msg:string -> t -> t -> unit

    (** Create [t] using of list *)
    val of_list : e list -> t
  end

(** {2 Implementations} *)

(** Collection of elements based on a Set, elements order doesn't matter *)
module SetMake : functor (D : DIFF_ELEMENT) -> S 
  with type e = D.t

(** Collection of elements based on a List, order matters but difference display
    is very simple. It stops at the first element which differs.
  *)
module ListSimpleMake : functor (D: DIFF_ELEMENT) -> S 
  with type e = D.t and type t = D.t list

val pp_comma_separator : Format.formatter -> unit -> unit
