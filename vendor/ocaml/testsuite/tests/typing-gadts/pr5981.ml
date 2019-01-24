module F(S : sig type 'a t end) = struct
  type _ ab =
      A : int S.t ab
    | B : float S.t ab

  let f : int S.t ab -> float S.t ab -> string =
    fun (l : int S.t ab) (r : float S.t ab) -> match l, r with
    | A, B -> "f A B"
end;;
[%%expect{|
Line _, characters 47-84:
Warning 8: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
(A, A)
module F :
  functor (S : sig type 'a t end) ->
    sig
      type _ ab = A : int S.t ab | B : float S.t ab
      val f : int S.t ab -> float S.t ab -> string
    end
|}];;

module F(S : sig type 'a t end) = struct
  type a = int * int
  type b = int -> int

  type _ ab =
      A : a S.t ab
    | B : b S.t ab

  let f : a S.t ab -> b S.t ab -> string =
    fun l r -> match l, r with
    | A, B -> "f A B"
end;;
[%%expect{|
Line _, characters 15-52:
Warning 8: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
(A, A)
module F :
  functor (S : sig type 'a t end) ->
    sig
      type a = int * int
      type b = int -> int
      type _ ab = A : a S.t ab | B : b S.t ab
      val f : a S.t ab -> b S.t ab -> string
    end
|}];;
