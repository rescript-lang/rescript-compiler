type t = A of {x:int; mutable y:int};;
let f (A r) = r;;  (* -> escape *)
let f (A r) = r.x;; (* ok *)
let f x = A {x; y = x};; (* ok *)
let f (A r) = A {r with y = r.x + 1};; (* ok *)
let f () = A {a = 1};; (* customized error message *)
let f () = A {x = 1; y = 3};; (* ok *)

type _ t = A: {x : 'a; y : 'b} -> 'a t;;
let f (A {x; y}) = A {x; y = ()};;  (* ok *)
let f (A ({x; y} as r)) = A {x = r.x; y = r.y};; (* ok *)

module M = struct
  type 'a t =
    | A of {x : 'a}
    | B: {u : 'b} -> unit t;;

  exception Foo of {x : int};;
end;;

module N : sig
  type 'b t = 'b M.t =
    | A of {x : 'b}
    | B: {u : 'bla} -> unit t

  exception Foo of {x : int}
end = struct
  type 'b t = 'b M.t =
    | A of {x : 'b}
    | B: {u : 'z} -> unit t

  exception Foo = M.Foo
end;;


module type S = sig exception A of {x:int}  end;;

module F (X : sig val x : (module S) end) = struct
  module A = (val X.x)
end;;  (* -> this expression creates fresh types (not really!) *)


module type S = sig
  exception A of {x : int}
  exception A of {x : string}
end;;

module M = struct
  exception A of {x : int}
  exception A of {x : string}
end;;


module M1 = struct
  exception A of {x : int}
end;;

module M = struct
  include M1
  include M1
end;;


module type S1 = sig
  exception A of {x : int}
end;;

module type S = sig
  include S1
  include S1
end;;

module M = struct
  exception A = M1.A
end;;

module X1 = struct
  type t = ..
end;;
module X2 = struct
  type t = ..
end;;
module Z = struct
  type X1.t += A of {x: int}
  type X2.t += A of {x: int}
end;;

(* PR#6716 *)

type _ c = C : [`A] c
type t = T : {x:[<`A] c} -> t;;
let f (T { x = C }) = ();;
