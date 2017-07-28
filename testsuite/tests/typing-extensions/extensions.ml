
type foo = ..
;;

type foo +=
    A
  | B of int
;;

let is_a x =
  match x with
    A -> true
  | _ -> false
;;

(* The type must be open to create extension *)

type foo
;;

type foo += A of int (* Error type is not open *)
;;

(* The type parameters must match *)

type 'a foo = ..
;;

type ('a, 'b) foo += A of int (* Error: type parameter mismatch *)
;;

(* In a signature the type does not have to be open *)

module type S =
sig
  type foo
  type foo += A of float
end
;;

(* But it must still be extensible *)

module type S =
sig
  type foo = A of int
  type foo += B of float (* Error foo does not have an extensible type *)
end
;;

(* Signatures can change the grouping of extensions *)

type foo = ..
;;

module M = struct
  type foo +=
      A of int
    | B of string

  type foo +=
      C of int
    | D of float
end
;;

module type S = sig
  type foo +=
      B of string
    | C of int

  type foo += D of float

  type foo += A of int
end
;;

module M_S = (M : S)
;;

(* Extensions can be GADTs *)

type 'a foo = ..
;;

type _ foo +=
    A : int -> int foo
  | B : int foo
;;

let get_num : type a. a foo -> a -> a option = fun f i1 ->
    match f with
        A i2 -> Some (i1 + i2)
     |  _ -> None
;;

(* Extensions must obey constraints *)

type 'a foo = .. constraint 'a = [> `Var ]
;;

type 'a foo += A of 'a
;;

let a = A 9 (* ERROR: Constraints not met *)
;;

type 'a foo += B : int foo (* ERROR: Constraints not met *)
;;

(* Signatures can make an extension private *)

type foo = ..
;;

module M = struct type foo += A of int end
;;

let a1 = M.A 10
;;

module type S = sig type foo += private A of int end
;;

module M_S = (M : S)
;;

let is_s x =
  match x with
    M_S.A _ -> true
  | _ -> false
;;

let a2 = M_S.A 20 (* ERROR: Cannot create a value using a private constructor *)
;;

(* Extensions can be rebound *)

type foo = ..
;;

module M = struct type foo += A1 of int end
;;

type foo += A2 = M.A1
;;

type bar = ..
;;

type bar += A3 = M.A1    (* Error: rebind wrong type *)
;;

module M = struct type foo += private B1 of int end
;;

type foo += private B2 = M.B1
;;

type foo += B3 = M.B1  (* Error: rebind private extension *)
;;

type foo += C = Unknown  (* Error: unbound extension *)
;;

(* Extensions can be rebound even if type is closed *)

module M : sig type foo type foo += A1 of int end
  = struct type foo = .. type foo += A1 of int end

type M.foo += A2 = M.A1

(* Rebinding handles abbreviations *)

type 'a foo = ..
;;

type 'a foo1 = 'a foo = ..
;;

type 'a foo2 = 'a foo = ..
;;

type 'a foo1 +=
    A of int
  | B of 'a
  | C : int foo1
;;

type 'a foo2 +=
    D = A
  | E = B
  | F = C
;;

(* Extensions must obey variances *)

type +'a foo = ..
;;

type 'a foo += A of (int -> 'a)
;;

type 'a foo += B of ('a -> int) (* ERROR: Parameter variances are not satisfied *)
;;

type _ foo += C : ('a -> int) -> 'a foo (* ERROR: Parameter variances are not satisfied *)
;;

type 'a bar = ..
;;

type +'a bar += D of (int -> 'a) (* ERROR: type variances do not match *)
;;

(* Exceptions are compatible with extensions *)

module M : sig
  type exn +=
      Foo of int * float
    | Bar : 'a list -> exn
end = struct
  exception Bar : 'a list -> exn
  exception Foo of int * float
end
;;

module M : sig
  exception Bar : 'a list -> exn
  exception Foo of int * float
end = struct
  type exn +=
      Foo of int * float
    | Bar : 'a list -> exn
end
;;

exception Foo of int * float
;;

exception Bar : 'a list -> exn
;;

module M : sig
  type exn +=
      Foo of int * float
    | Bar : 'a list -> exn
end = struct
  exception Bar = Bar
  exception Foo = Foo
end
;;

(* Test toplevel printing *)

type foo = ..
;;

type foo +=
    Foo of int * int option
  | Bar of int option
;;

let x = Foo(3, Some 4), Bar(Some 5) (* Prints Foo and Bar successfully *)
;;

type foo += Foo of string
;;

let y = x (* Prints Bar but not Foo (which has been shadowed) *)
;;

exception Foo of int * int option
;;

exception Bar of int option
;;

let x = Foo(3, Some 4), Bar(Some 5) (* Prints Foo and Bar successfully *)
;;

type foo += Foo of string
;;

let y = x (* Prints Bar and part of Foo (which has been shadowed) *)
;;

(* Test Obj functions *)

type foo = ..
;;

type foo +=
    Foo
  | Bar of int
;;

let n1 = Obj.extension_name Foo
;;

let n2 = Obj.extension_name (Bar 1)
;;

let t = (Obj.extension_id (Bar 2)) = (Obj.extension_id (Bar 3)) (* true *)
;;

let f = (Obj.extension_id (Bar 2)) = (Obj.extension_id Foo) (* false *)
;;

let is_foo x = (Obj.extension_id Foo) = (Obj.extension_id x)

type foo += Foo
;;

let f = is_foo Foo
;;

let _ = Obj.extension_name 7 (* Invald_arg *)
;;

let _ = Obj.extension_id (object method m = 3 end) (* Invald_arg *)
;;
