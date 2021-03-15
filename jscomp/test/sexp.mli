(*
Copyright (c) 2013, Simon Cruanes
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  Redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(** {1 Handling S-expressions}

    @since 0.4

    @since 0.7
    Moved the streaming parser to CCSexpStream
*)

(** {2 Basics} *)

type t = [
  | `Atom of string
  | `List of t list
]

val equal : t -> t -> bool
val compare : t -> t -> int
val hash : t -> int

val atom : string -> t  (** Build an atom directly from a string *)

val of_int : int -> t
val of_bool : bool -> t
val of_list : t list -> t
val of_rev_list : t list -> t  (** Reverse the list *)
val of_float : float -> t
val of_unit : t
val of_pair : t * t -> t
val of_triple : t * t * t -> t
val of_quad : t * t * t * t -> t

val of_variant : string -> t list -> t
(** [of_variant name args] is used to encode algebraic variants
    into a S-expr. For instance [of_variant "some" [of_int 1]]
    represents the value [Some 1] *)

val of_field : string -> t -> t
(** Used to represent one record field *)

val of_record : (string * t) list -> t
(** Represent a record by its named fields *)

(** {6 Traversal of S-exp}

    Example: serializing 2D points
    {[
      type pt = {x:int; y:int };;

      let pt_of_sexp e =
        Sexp.Traverse.(
          field "x" to_int e >>= fun x ->
          field "y" to_int e >>= fun y ->
          return {x;y}
        );;

      let sexp_of_pt pt = Sexp.(of_record ["x", of_int pt.x; "y", of_int pt.y]);;

      let l = [{x=1;y=1}; {x=2;y=10}];;

      let sexp = Sexp.(of_list (List.map sexp_of_pt l));;

      Sexp.Traverse.list_all pt_of_sexp sexp;;
    ]}

*)

module Traverse : sig
  type 'a conv = t -> 'a option
  (** A converter from S-expressions to 'a is a function [sexp -> 'a option].
      @since 0.4.1 *)

  val map_opt : ('a -> 'b option) -> 'a list -> 'b list option
  (** Map over a list, failing as soon as the function fails on any element
      @since 0.4.1 *)

  val list_any : 'a conv -> t -> 'a option
  (** [list_any f (List l)] tries [f x] for every element [x] in [List l],
      and returns the first non-None result (if any). *)

  val list_all : 'a conv -> t -> 'a list
  (** [list_all f (List l)] returns the list of all [y] such that [x] in [l]
      and [f x = Some y] *)

  val to_int : int conv
  (** Expect an integer *)

  val to_string : string conv
  (** Expect a string (an atom) *)

  val to_bool : bool conv
  (** Expect a boolean *)

  val to_float : float conv
  (** Expect a float *)

  val to_list : t list conv
  (** Expect a list *)

  val to_list_with : (t -> 'a option) -> 'a list conv
  (** Expect a list, applies [f] to all the elements of the list, and succeeds
      only if [f] succeeded on every element
      @since 0.4.1 *)

  val to_pair : (t * t) conv
  (** Expect a list of two elements *)

  val to_pair_with : 'a conv -> 'b conv -> ('a * 'b) conv
  (** Same as {!to_pair} but applies conversion functions
      @since 0.4.1 *)

  val to_triple : (t * t * t) conv

  val to_triple_with : 'a conv -> 'b conv -> 'c conv -> ('a * 'b * 'c) conv
  (* @since 0.4.1 *)

  val get_field : string -> t conv
  (** [get_field name e], when [e = List [(n1,x1); (n2,x2) ... ]], extracts
      the [xi] such that [name = ni], if it can find it. *)

  val field : string -> 'a conv -> 'a conv
  (** Enriched version of {!get_field}, with a converter as argument *)

  val get_variant : (string * (t list -> 'a option)) list -> 'a conv
  (** [get_variant l e] checks whether [e = List (Atom s :: args)], and
      if some pair of [l] is [s, f]. In this case, it calls [f args]
      and returns its result, otherwise it returns None. *)

  val field_list : string -> (t list -> 'a option) -> 'a conv
  (** [field_list name f  "(... (name a b c d) ...record)"]
      will look for a field based on the given [name], and expect it to have a
      list of arguments dealt with by [f] (here, "a b c d").
      @since 0.4.1 *)

  val (>>=) : 'a option -> ('a -> 'b option) -> 'b option

  val (>|=) : 'a option -> ('a -> 'b) -> 'b option

  val return : 'a -> 'a option

  val get_exn : 'a option -> 'a
  (** Unwrap an option, possibly failing.
      @raise Invalid_argument if the argument is [None] *)
end
