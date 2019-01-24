(*

A testbed file for private type abbreviation definitions.

We define a Length module to implement positive integers.

*)

type t = private int;;

val make : int -> t;;

external from : t -> int = "%identity";;
