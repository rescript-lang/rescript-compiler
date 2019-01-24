(* PR#7012 *)

type t = [ 'A_name | `Hi ];;
[%%expect{|
Line _, characters 11-18:
Error: The type 'A_name does not expand to a polymorphic variant type
Hint: Did you mean `A_name?
|}];;

let f (x:'id_arg) = x;;
[%%expect{|
val f : 'id_arg -> 'id_arg = <fun>
|}];;

let f (x:'Id_arg) = x;;
[%%expect{|
val f : 'Id_arg -> 'Id_arg = <fun>
|}];;

(* GPR#1204, GPR#1329 *)
type 'a id = 'a
let f (x : [< [`Foo] id]) = ();;
[%%expect{|
type 'a id = 'a
val f : [< [ `Foo ] id ] -> unit = <fun>
|}];;

module M = struct module N = struct type t = [`A] end end;;
let f x = (x :> M.N.t);;
[%%expect{|
module M : sig module N : sig type t = [ `A ] end end
val f : [< M.N.t ] -> M.N.t = <fun>
|}]
module G = M.N;;
let f x = (x :> G.t);;
[%%expect{|
module G = M.N
val f : [< G.t ] -> G.t = <fun>
|}]
