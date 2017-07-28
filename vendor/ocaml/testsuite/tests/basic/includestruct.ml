(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Test for "include <module-expr>" inside structures *)

module A =
  struct
    type t = int
    let x = (1 : t)
    let y = (2 : t)
    let f (z : t) = (x + z : t)
  end

module B =
  struct
    include A
    type u = t * t
    let p = ((x, y) : u)
    let g ((x, y) : u) = ((f x, f y) : u)
  end

let _ =
  let print_pair (x,y) =
    print_int x; print_string ", "; print_int y; print_newline() in
  print_pair B.p;
  print_pair (B.g B.p);
  print_pair (B.g (123, 456))

module H =
  struct
    include A
    let f (z : t) = (x - 1 : t)
  end

let _ =
  print_int (H.f H.x); print_newline()

module C =
  struct
    include (A : sig type t val f : t -> int val x : t end)
    let z = f x
  end

let _ =
  print_int C.z; print_newline();
  print_int (C.f C.x); print_newline()

(* Toplevel inclusion *)

include A

let _ =
  print_int x; print_newline();
  print_int (f y); print_newline()

(* With a functor *)

module F(X: sig end) =
  struct
    let _ = print_string "F is called"; print_newline()
    type t = A | B of int
    let print_t = function A -> print_string "A"
                         | B x -> print_int x
  end

module D =
  struct
    include F(struct end)
    let test() = print_t A; print_newline(); print_t (B 42); print_newline()
  end

let _ =
  D.test();
  D.print_t D.A; print_newline(); D.print_t (D.B 42); print_newline()

(* Exceptions and classes *)

module E =
  struct
    exception Exn of string
    class c = object method m = 1 end
  end

module G =
  struct
    include E
    let _ =
      begin try raise (Exn "foo") with Exn s -> print_string s end;
      print_int ((new c)#m); print_newline()
  end

let _ =
  begin try raise (G.Exn "foo") with G.Exn s -> print_string s end;
  print_int ((new G.c)#m); print_newline()
