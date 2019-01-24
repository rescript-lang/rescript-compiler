module X = struct
  type t =
    | A : 'a * 'b * ('a -> unit) -> t
end;;
[%%expect{|
module X : sig type t = A : 'a * 'b * ('a -> unit) -> t end
|}]

module Y = struct
  type t = X.t =
    | A : 'a * 'b * ('b -> unit) -> t
end;; (* should fail *)
[%%expect{|
Line _, characters 2-54:
Error: This variant or record definition does not match that of type X.t
       The types for field A are not equal.
|}]

(* would segfault
let () =
  match Y.A (1, "", print_string) with
  | X.A (x, y, f) -> f x
*)
