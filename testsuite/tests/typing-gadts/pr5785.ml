module Add (T : sig type two end) =
struct
  type _ t =
  | One : [`One] t
  | Two : T.two t

  let add (type a) : a t * a t -> string = function
    | One, One -> "two"
    | Two, Two -> "four"
end;;
