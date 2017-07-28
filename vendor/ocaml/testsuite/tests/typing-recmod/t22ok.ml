(* Tests for recursive modules *)

let test number result expected =
  if result = expected
  then Printf.printf "Test %d passed.\n" number
  else Printf.printf "Test %d FAILED.\n" number;
  flush stdout

(* Tree of sets *)

module rec A
 : sig
     type t = Leaf of int | Node of ASet.t
     val compare: t -> t -> int
   end
 = struct
     type t = Leaf of int | Node of ASet.t
     let compare x y =
       match (x,y) with
         (Leaf i, Leaf j) -> Pervasives.compare i j
       | (Leaf i, Node t) -> -1
       | (Node s, Leaf j) -> 1
       | (Node s, Node t) -> ASet.compare s t
   end

and ASet : Set.S with type elt = A.t = Set.Make(A)
;;

let _ =
  let x = A.Node (ASet.add (A.Leaf 3) (ASet.singleton (A.Leaf 2))) in
  let y = A.Node (ASet.add (A.Leaf 1) (ASet.singleton x)) in
  test 10 (A.compare x x) 0;
  test 11 (A.compare x (A.Leaf 3)) 1;
  test 12 (A.compare (A.Leaf 0) x) (-1);
  test 13 (A.compare y y) 0;
  test 14 (A.compare x y) 1
;;

(* Simple value recursion *)

module rec Fib
  : sig val f : int -> int end
  = struct let f x = if x < 2 then 1 else Fib.f(x-1) + Fib.f(x-2) end
;;

let _ =
  test 20 (Fib.f 10) 89
;;

(* Update function by infix *)

module rec Fib2
  : sig val f : int -> int end
  = struct let rec g x = Fib2.f(x-1) + Fib2.f(x-2)
               and f x = if x < 2 then 1 else g x
    end
;;

let _ =
  test 21 (Fib2.f 10) 89
;;

(* Early application *)

let _ =
  let res =
    try
      let module A =
        struct
          module rec Bad
            : sig val f : int -> int end
            = struct let f = let y = Bad.f 5 in fun x -> x+y end
          end in
      false
    with Undefined_recursive_module _ ->
      true in
  test 30 res true
;;

(* Early strict evaluation *)

(*
module rec Cyclic
  : sig val x : int end
  = struct let x = Cyclic.x + 1 end
;;
*)

(* Reordering of evaluation based on dependencies *)

module rec After
  : sig val x : int end
  = struct let x = Before.x + 1 end
and Before
  : sig val x : int end
  = struct let x = 3 end
;;

let _ =
  test 40 After.x 4
;;

(* Type identity between A.t and t within A's definition *)

module rec Strengthen
  : sig type t val f : t -> t end
  = struct
      type t = A | B
      let _ = (A : Strengthen.t)
      let f x = if true then A else Strengthen.f B
    end
;;

module rec Strengthen2
  : sig type t
        val f : t -> t
        module M : sig type u end
        module R : sig type v end
    end
  = struct
      type t = A | B
      let _ = (A : Strengthen2.t)
      let f x = if true then A else Strengthen2.f B
      module M =
        struct
          type u = C
          let _ = (C: Strengthen2.M.u)
        end
      module rec R : sig type v  = Strengthen2.R.v end =
        struct
          type v = D
          let _ = (D : R.v)
          let _ = (D : Strengthen2.R.v)
        end
    end
;;

(* Polymorphic recursion *)

module rec PolyRec
  : sig
      type 'a t = Leaf of 'a | Node of 'a list t * 'a list t
      val depth: 'a t -> int
    end
  = struct
      type 'a t = Leaf of 'a | Node of 'a list t * 'a list t
      let x = (PolyRec.Leaf 1 : int t)
      let depth = function
        Leaf x -> 0
      | Node(l,r) -> 1 + max (PolyRec.depth l) (PolyRec.depth r)
    end
;;

(* Wrong LHS signatures (PR#4336) *)

(*
module type ASig = sig type a val a:a val print:a -> unit end
module type BSig = sig type b val b:b val print:b -> unit end

module A = struct type a = int let a = 0 let print = print_int end
module B = struct type b = float let b = 0.0 let print = print_float end

module MakeA (Empty:sig end) : ASig = A
module MakeB (Empty:sig end) : BSig = B

module
   rec NewA : ASig = MakeA (struct end)
   and NewB : BSig with type b = NewA.a = MakeB (struct end);;

*)

(* Expressions and bindings *)

module StringSet = Set.Make(String);;

module rec Expr
  : sig
      type t =
        Var of string
      | Const of int
      | Add of t * t
      | Binding of Binding.t * t
      val make_let: string -> t -> t -> t
      val fv: t -> StringSet.t
      val simpl: t -> t
    end
  = struct
      type t =
        Var of string
      | Const of int
      | Add of t * t
      | Binding of Binding.t * t
      let make_let id e1 e2 = Binding([id, e1], e2)
      let rec fv = function
        Var s -> StringSet.singleton s
      | Const n -> StringSet.empty
      | Add(t1,t2) -> StringSet.union (fv t1) (fv t2)
      | Binding(b,t) ->
          StringSet.union (Binding.fv b)
            (StringSet.diff (fv t) (Binding.bv b))
      let rec simpl = function
        Var s -> Var s
      | Const n -> Const n
      | Add(Const i, Const j) -> Const (i+j)
      | Add(Const 0, t) -> simpl t
      | Add(t, Const 0) -> simpl t
      | Add(t1,t2) -> Add(simpl t1, simpl t2)
      | Binding(b, t) -> Binding(Binding.simpl b, simpl t)
    end

and Binding
  : sig
      type t = (string * Expr.t) list
      val fv: t -> StringSet.t
      val bv: t -> StringSet.t
      val simpl: t -> t
    end
  = struct
      type t = (string * Expr.t) list
      let fv b =
        List.fold_left (fun v (id,e) -> StringSet.union v (Expr.fv e))
                       StringSet.empty b
      let bv b =
        List.fold_left (fun v (id,e) -> StringSet.add id v)
                       StringSet.empty b
      let simpl b =
        List.map (fun (id,e) -> (id, Expr.simpl e)) b
    end
;;

let _ =
  let e = Expr.make_let "x" (Expr.Add (Expr.Var "y", Expr.Const 0))
                            (Expr.Var "x") in
  let e' = Expr.make_let "x" (Expr.Var "y") (Expr.Var "x") in
  test 50 (StringSet.elements (Expr.fv e)) ["y"];
  test 51 (Expr.simpl e) e'
;;

(* Okasaki's bootstrapping *)

module type ORDERED =
  sig
    type t
    val eq: t -> t -> bool
    val lt: t -> t -> bool
    val leq: t -> t -> bool
  end

module type HEAP =
  sig
    module Elem: ORDERED
    type heap
    val empty: heap
    val isEmpty: heap -> bool
    val insert: Elem.t -> heap -> heap
    val merge: heap -> heap -> heap
    val findMin: heap -> Elem.t
    val deleteMin: heap -> heap
  end

module Bootstrap (MakeH: functor (Element:ORDERED) ->
                                    HEAP with module Elem = Element)
                 (Element: ORDERED) : HEAP with module Elem = Element =
  struct
    module Elem = Element
    module rec BE
    : sig type t = E | H of Elem.t * PrimH.heap
          val eq: t -> t -> bool
          val lt: t -> t -> bool
          val leq: t -> t -> bool
      end
    = struct
        type t = E | H of Elem.t * PrimH.heap
        let leq t1 t2 =
          match t1, t2 with
          | (H(x, _)), (H(y, _)) -> Elem.leq x y
          | H _, E -> false
          | E, H _ -> true
          | E, E -> true
        let eq t1 t2 =
          match t1, t2 with
          | (H(x, _)), (H(y, _)) -> Elem.eq x y
          | H _, E -> false
          | E, H _ -> false
          | E, E -> true
        let lt t1 t2 =
          match t1, t2 with
          | (H(x, _)), (H(y, _)) -> Elem.lt x y
          | H _, E -> false
          | E, H _ -> true
          | E, E -> false
      end
    and PrimH
    : HEAP with type Elem.t = BE.t
    = MakeH(BE)
    type heap = BE.t
    let empty = BE.E
    let isEmpty = function BE.E -> true | _ -> false
    let rec merge x y =
      match (x,y) with
        (BE.E, _) -> y
      | (_, BE.E) -> x
      | (BE.H(e1,p1) as h1), (BE.H(e2,p2) as h2) ->
          if Elem.leq e1 e2
          then BE.H(e1, PrimH.insert h2 p1)
          else BE.H(e2, PrimH.insert h1 p2)
    let insert x h =
      merge (BE.H(x, PrimH.empty)) h
    let findMin = function
        BE.E -> raise Not_found
      | BE.H(x, _) -> x
    let deleteMin = function
        BE.E -> raise Not_found
      | BE.H(x, p) ->
          if PrimH.isEmpty p then BE.E else begin
            match PrimH.findMin p with
            | (BE.H(y, p1)) ->
              let p2 = PrimH.deleteMin p in
              BE.H(y, PrimH.merge p1 p2)
            | BE.E -> assert false
          end
  end
;;

module LeftistHeap(Element: ORDERED): HEAP with module Elem = Element =
  struct
    module Elem = Element
    type heap = E | T of int * Elem.t * heap * heap
    let rank = function E -> 0 | T(r,_,_,_) -> r
    let make x a b =
      if rank a >= rank b
      then T(rank b + 1, x, a, b)
      else T(rank a + 1, x, b, a)
    let empty = E
    let isEmpty = function E -> true | _ -> false
    let rec merge h1 h2 =
      match (h1, h2) with
        (_, E) -> h1
      | (E, _) -> h2
      | (T(_, x1, a1, b1), T(_, x2, a2, b2)) ->
          if Elem.leq x1 x2
          then make x1 a1 (merge b1 h2)
          else make x2 a2 (merge h1 b2)
    let insert x h = merge (T(1, x, E, E)) h
    let findMin = function
      E -> raise Not_found
    | T(_, x, _, _) -> x
    let deleteMin = function
      E -> raise Not_found
    | T(_, x, a, b) -> merge a b
  end
;;

module Ints =
  struct
    type t = int
    let eq = (=)
    let lt = (<)
    let leq = (<=)
  end
;;

module C = Bootstrap(LeftistHeap)(Ints);;

let _ =
  let h = List.fold_right C.insert [6;4;8;7;3;1] C.empty in
  test 60 (C.findMin h) 1;
  test 61 (C.findMin (C.deleteMin h)) 3;
  test 62 (C.findMin (C.deleteMin (C.deleteMin h))) 4
;;

(* Classes *)

module rec Class1
  : sig
      class c : object method m : int -> int end
    end
  = struct
      class c =
        object
          method m x = if x <= 0 then x else (new Class2.d)#m x
        end
    end
and Class2
  : sig
      class d : object method m : int -> int end
    end
  = struct
      class d =
        object(self)
          inherit Class1.c as super
          method m (x:int) = super#m 0
        end
    end
;;

let _ =
  test 70 ((new Class1.c)#m 7) 0
;;

let _ =
  try
    let module A = struct
       module rec BadClass1
         : sig
             class c : object method m : int end
           end
         = struct
             class c = object method m = 123 end
           end
       and BadClass2
         : sig
             val x: int
           end
         = struct
             let x = (new BadClass1.c)#m
           end
    end in
      test 71 true false
  with Undefined_recursive_module _ ->
    test 71 true true
;;

(* Coercions *)

module rec Coerce1
  : sig
      val g: int -> int
      val f: int -> int
    end
  = struct
      module A = (Coerce1: sig val f: int -> int end)
      let g x = x
      let f x = if x <= 0 then 1 else A.f (x-1) * x
    end
;;

let _ =
  test 80 (Coerce1.f 10) 3628800
;;

module CoerceF(S: sig end) = struct
  let f1 () = 1
  let f2 () = 2
  let f3 () = 3
  let f4 () = 4
  let f5 () = 5
end

module rec Coerce2: sig val f1: unit -> int end = CoerceF(Coerce3)
       and Coerce3: sig end = struct end
;;

let _ =
  test 81 (Coerce2.f1 ()) 1
;;

module Coerce4(A : sig val f : int -> int end) = struct
  let x = 0
  let at a = A.f a
end

module rec Coerce5
  : sig val blabla: int -> int val f: int -> int end
  = struct let blabla x = 0 let f x = 5 end
and Coerce6
  : sig val at: int -> int end
  = Coerce4(Coerce5)

let _ =
  test 82 (Coerce6.at 100) 5
;;

(* Miscellaneous bug reports *)

module rec F
  : sig type t = X of int | Y of int
        val f: t -> bool
    end
  = struct
      type t = X of int | Y of int
      let f = function
        | X _ -> false
        | _ -> true
    end;;

let _ =
  test 100 (F.f (F.X 1)) false;
  test 101 (F.f (F.Y 2)) true

(* PR#4316 *)
module G(S : sig val x : int Lazy.t end) = struct include S end

module M1 = struct let x = lazy 3 end

let _ = Lazy.force M1.x

module rec M2 : sig val x : int Lazy.t end = G(M1)

let _ =
  test 102 (Lazy.force M2.x) 3

let _ = Gc.full_major()   (* will shortcut forwarding in M1.x *)

module rec M3 : sig val x : int Lazy.t end = G(M1)

let _ =
  test 103 (Lazy.force M3.x) 3


(** Pure type-checking tests: see recmod/*.ml  *)
