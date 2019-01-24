module Add (T : sig type two end) =
struct
  type _ t =
  | One : [`One] t
  | Two : T.two t

  let add (type a) : a t * a t -> string = function
    | One, One -> "two"
    | Two, Two -> "four"
end;;
[%%expect{|
Line _, characters 43-100:
Warning 8: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
(Two, One)
module Add :
  functor (T : sig type two end) ->
    sig
      type _ t = One : [ `One ] t | Two : T.two t
      val add : 'a t * 'a t -> string
    end
|}];;
