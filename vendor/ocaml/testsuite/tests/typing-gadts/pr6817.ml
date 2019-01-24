module A = struct
    type nil = Cstr
  end
open A
;;

type _ s =
  | Nil : nil s
  | Cons : 't s -> ('h -> 't) s

type ('stack, 'typ) var =
  | Head : (('typ -> _) s, 'typ) var
  | Tail : ('tail s, 'typ) var -> ((_ -> 'tail) s, 'typ) var

type _ lst =
  | CNil : nil lst
  | CCons : 'h * ('t lst) -> ('h -> 't) lst
;;

let rec get_var : type stk ret. (stk s, ret) var -> stk lst -> ret = fun n s ->
  match n, s with
  | Head, CCons (h, _) -> h
  | Tail n', CCons (_, t) -> get_var n' t
;;

[%%expect{|
module A : sig type nil = Cstr end
type _ s = Nil : A.nil s | Cons : 't s -> ('h -> 't) s
type ('stack, 'typ) var =
    Head : (('typ -> 'a) s, 'typ) var
  | Tail : ('tail s, 'typ) var -> (('b -> 'tail) s, 'typ) var
type _ lst = CNil : A.nil lst | CCons : 'h * 't lst -> ('h -> 't) lst
val get_var : ('stk s, 'ret) var -> 'stk lst -> 'ret = <fun>
|}];;
