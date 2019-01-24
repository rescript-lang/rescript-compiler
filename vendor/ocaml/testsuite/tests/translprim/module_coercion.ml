module M = struct
  external len : 'a array -> int = "%array_length"
  external safe_get : 'a array -> int -> 'a = "%array_safe_get"
  external unsafe_get : 'a array -> int -> 'a = "%array_unsafe_get"
  external safe_set : 'a array -> int -> 'a -> unit = "%array_safe_set"
  external unsafe_set : 'a array -> int -> 'a -> unit = "%array_unsafe_set"
  external cmp : 'a -> 'a -> int = "%compare";;
  external eq : 'a -> 'a -> bool = "%equal";;
  external ne : 'a -> 'a -> bool = "%notequal";;
  external lt : 'a -> 'a -> bool = "%lessthan";;
  external gt : 'a -> 'a -> bool = "%greaterthan";;
  external le : 'a -> 'a -> bool = "%lessequal";;
  external ge : 'a -> 'a -> bool = "%greaterequal";;
end;;

module type T = sig
  type t
  val len : t array -> int
  val safe_get : t array -> int -> t
  val unsafe_get : t array -> int -> t
  val safe_set : t array -> int -> t -> unit
  val unsafe_set : t array -> int -> t -> unit
  val cmp : t -> t -> int
  val eq : t -> t -> bool
  val ne : t -> t -> bool
  val lt : t -> t -> bool
  val gt : t -> t -> bool
  val le : t -> t -> bool
  val ge : t -> t -> bool
end;;

module M_int : T with type t := int = M;;
module M_float : T with type t := float = M;;
module M_string : T with type t := string = M;;
module M_int32 : T with type t := int32 = M;;
module M_int64 : T with type t := int64 = M;;
module M_nativeint : T with type t := nativeint = M;;
