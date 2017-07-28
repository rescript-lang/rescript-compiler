(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*          Damien Doligez, projet Gallium, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2013 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Exotic OCaml syntax constructs found in the manual that are not *)
(* used in the source of the OCaml distribution (even in the tests). *)

(* Spaces between the parts of the ?label: token in a typexpr.
   (used in bin-prot) *)
type t1 = ? label : int -> int -> int;;

(* Lazy in a pattern. (used in advi) *)
function lazy y -> y;;

(* Spaces between the parts of the ?label: token in a class-type. *)
class c1 =
  (fun ?label:x y -> object end : ? label : int -> int -> object end)
;;

(* type-class annotation in class-expr *)
class c2 = (object end : object end);;

(* virtual object field *)
class virtual c3 = object val virtual x : int end;;
class virtual c4 = object val mutable virtual x : int end;;

(* abstract module type in a signature *)
module type T = sig
  module type U
end;;

(* associativity rules for patterns *)
function Some Some x -> x | _ -> 0;;
function Some `Tag x -> x | _ -> 0;;
function `Tag Some x -> x | _ -> 0;;
function `Tag `Tag x -> x | _ -> 0;;

(* negative int32, int64, nativeint constants in patterns *)
function -1l -> () | _ -> ();;
function -1L -> () | _ -> ();;
function -1n -> () | _ -> ();;

(* surprising places where you can use an operator as a variable name *)
function (+) -> (+);;
function _ as (+) -> (+);;
for (+) = 0 to 1 do () done;;

(* access a class-type through an extended-module-path *)
module F (X : sig end) = struct
  class type t = object end
end;;
module M1 = struct end;;
class type u = F(M1).t;;

(* conjunctive constraints on tags (used by the compiler to print some
   inferred types) *)
type 'a t2 = [< `A of int & int & int ] as 'a;;

(* same for a parameterless tag (triggers a very strange error message) *)
(*type ('a, 'b) t3 = [< `A of & 'b ] as 'a;;*)

(* negative float constant in a pattern *)
function -1.0 -> 1 | _ -> 2;;

(* combining language extensions (sec. 7.13 and 7.17) *)
class c5 = object method f = 1 end;;
object
  inherit c5
  method! f : type t . int = 2
end;;

(* private polymorphic method with local type *)
object method private f : type t . int = 1 end;;


(* More exotic: not even found in the manual (up to version 4.00),
   but used in some programs found in the wild.
*)

(* local functor *)
let module M (M1 : sig end) = struct end in ();;

(* let-binding with a type coercion *)
let x :> int = 1;;
let x : int :> int = 1;;

(* "begin end" as an alias for "()" *)
begin end;;

(* putting "virtual" before "mutable" or "private" *)
class type virtual ct = object
  val mutable virtual x : int
  val virtual mutable y : int
  method private virtual f : int
  method virtual private g : int
end;;
class virtual c = object
  val mutable virtual x : int
  val virtual mutable y : int
  method private virtual f : int
  method virtual private g : int
end;;

(* Double-semicolon at the beginning of a module body [ocp-indent] *)
module M2 = struct ;; end;;


(**********************

(* Most exotic: not found in the manual (up to 4.00) and not used
   deliberately by anyone, but still implemented by the compiler. *)

(* whitespace inside val!, method!, inherit! [found in ocamlspot] *)
object
  val x = 1
  val ! x = 2
  method m = 1
  method ! m = 2
  inherit ! object val x = 3 end
end;;

(* Using () as a constructor name [found in gettext] *)
type t = ();;
let x : t = ();;

(* Using :: as a constructor name *)
type t = :: of int * int;;

(* Prefix syntax for :: in expressions *)
(::) (1, 1);;

(* Prefix syntax for :: in patterns *)
function (::) (_, _) -> 1;;

(* Unary plus in expressions (ints and float) *)
+1;;
+1l;;
+1L;;
+1n;;
+1.0;;

(* Unary plus in patterns (ints and floats) *)
function +1 -> ();;
function +1l -> ();;
function +1L -> ();;
function +1n -> ();;
function +1.0 -> ();;

**********************)
