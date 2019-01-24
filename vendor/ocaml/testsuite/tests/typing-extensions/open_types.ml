type foo = ..
;;

(* Check that abbreviations work *)

type bar = foo = ..
;;

type baz = foo = ..
;;

type bar += Bar1 of int
;;

type baz += Bar2 of int
;;

module M = struct type bar += Foo of float end
;;

module type S = sig type baz += Foo of float end
;;

module M_S = (M : S)
;;

(* Abbreviations need to be made open *)

type foo = ..
;;

type bar = foo
;;

type bar += Bar of int (* Error: type is not open *)
;;

type baz = bar = .. (* Error: type kinds don't match *)
;;

(* Abbreviations need to match parameters *)

type 'a foo = ..
;;

type ('a, 'b) bar = 'a foo = .. (* Error: arrities do not match *)
;;

type ('a, 'b) foo = ..
;;

type ('a, 'b) bar = ('a, 'a) foo = .. (* Error: constraints do not match *)
;;

(* Check that signatures can hide exstensibility *)

module M = struct type foo = .. end
;;

module type S = sig type foo end
;;

module M_S = (M : S)
;;

type M_S.foo += Foo (* ERROR: Cannot extend a type that isn't "open" *)
;;

(* Check that signatures cannot add extensibility *)

module M = struct type foo end
;;

module type S = sig type foo = .. end
;;

module M_S = (M : S) (* ERROR: Signatures are not compatible *)
;;

(* Check that signatures can make exstensibility private *)

module M = struct type foo = .. end
;;

module type S = sig type foo = private .. end
;;

module M_S = (M : S)
;;

type M_S.foo += Foo (* ERROR: Cannot extend a private extensible type *)
;;

(* Check that signatures cannot make private extensibility public *)

module M = struct type foo = private .. end
;;

module type S = sig type foo = .. end
;;

module M_S = (M : S) (* ERROR: Signatures are not compatible *)
;;


(* Check that signatures maintain variances *)

module M = struct type +'a foo = .. type 'a bar = 'a foo = .. end
;;

module type S = sig type 'a foo = .. type 'a bar = 'a foo = .. end
;;

module M_S = (M : S) (* ERROR: Signatures are not compatible *)
;;

(* Exn is an open type *)

type exn2 = exn = ..
;;

(* Exhaustiveness *)

type foo = ..
type foo += Foo
let f = function Foo -> ()
;; (* warn *)

(* More complex exhaustiveness *)

let f = function
  | [Foo] -> 1
  | _::_::_ -> 3
  | [] -> 2
;; (* warn *)


(* PR#7330: exhaustiveness with GADTs *)

type t = ..
type t += IPair : (int * int) -> t ;;

let f = function IPair (i, j) -> Format.sprintf "(%d, %d)" i j ;; (* warn *)
