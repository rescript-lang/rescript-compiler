type (_, _) eqp = Y : ('a, 'a) eqp | N : string -> ('a, 'b) eqp
let f : ('a list, 'a) eqp -> unit = function N s -> print_string s;;

module rec A :  sig type t = B.t list end =
  struct type t = B.t list end
and B : sig  type t val eq : (B.t list, t) eqp end =
  struct
    type t = A.t
    let eq = Y
  end;;

f B.eq;;

[%%expect{|
type (_, _) eqp = Y : ('a, 'a) eqp | N : string -> ('a, 'b) eqp
Line _, characters 36-66:
Warning 8: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
Y
val f : ('a list, 'a) eqp -> unit = <fun>
module rec A : sig type t = B.t list end
and B : sig type t val eq : (B.t list, t) eqp end
Exception: Match_failure ("", 2, 36).
|}];;
