module type Printable = sig
  type t
  val print : Format.formatter -> t -> unit
end;;
module type Comparable = sig
  type t
  val compare : t -> t -> int
end;;
module type PrintableComparable = sig
  include Printable
  include Comparable with type t = t
end;; (* Fails *)
module type PrintableComparable = sig
  type t
  include Printable with type t := t
  include Comparable with type t := t
end;;
module type PrintableComparable = sig
  include Printable
  include Comparable with type t := t
end;;
module type ComparableInt = Comparable with type t := int;;
module type S = sig type t val f : t -> t end;;
module type S' = S with type t := int;;

module type S = sig type 'a t val map : ('a -> 'b) -> 'a t -> 'b t end;;
module type S1 = S with type 'a t := 'a list;;
module type S2 = sig
  type 'a dict = (string * 'a) list
  include S with type 'a t := 'a dict
end;;


module type S =
  sig module T : sig type exp type arg end val f : T.exp -> T.arg end;;
module M = struct type exp = string type arg = int end;;
module type S' = S with module T := M;;


module type S = sig type 'a t end with type 'a t := unit;; (* Fails *)
