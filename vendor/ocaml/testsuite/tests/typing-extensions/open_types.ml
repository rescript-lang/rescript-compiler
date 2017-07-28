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

(* Private abstract types cannot be open *)

type foo = ..
;;

type bar = private foo = .. (* ERROR: Private abstract types cannot be open *)
;;

(* Check that signatures can hide open-ness *)

module M = struct type foo = .. end
;;

module type S = sig type foo end
;;

module M_S = (M : S)
;;

type M_S.foo += Foo (* ERROR: Cannot extend a type that isn't "open" *)
;;

(* Check that signatures cannot add open-ness *)

module M = struct type foo end
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
