(*
   Implicit unpack allows to omit the signature in (val ...) expressions.

   It also adds (module M : S) and (module M) patterns, relying on
   implicit (val ...) for the implementation. Such patterns can only
   be used in function definition, match clauses, and let ... in.

   New: implicit pack is also supported, and you only need to be able
   to infer the the module type path from the context.
 *)
(* ocaml -principal *)

(* Use a module pattern *)
let sort (type s) (module Set : Set.S with type elt = s) l =
  Set.elements (List.fold_right Set.add l Set.empty)

(* No real improvement here? *)
let make_set (type s) cmp : (module Set.S with type elt = s) =
  (module Set.Make (struct type t = s let compare = cmp end))

(* No type annotation here *)
let sort_cmp (type s) cmp =
  sort (module Set.Make (struct type t = s let compare = cmp end))

module type S = sig type t val x : t end;;
let f (module M : S with type t = int) = M.x;;
let f (module M : S with type t = 'a) = M.x;; (* Error *)
let f (type a) (module M : S with type t = a) = M.x;;
f (module struct type t = int let x = 1 end);;

type 'a s = {s: (module S with type t = 'a)};;
{s=(module struct type t = int let x = 1 end)};;
let f {s=(module M)} = M.x;; (* Error *)
let f (type a) ({s=(module M)} : a s) = M.x;;

type s = {s: (module S with type t = int)};;
let f {s=(module M)} = M.x;;
let f {s=(module M)} {s=(module N)} = M.x + N.x;;

module type S = sig val x : int end;;
let f (module M : S) y (module N : S) = M.x + y + N.x;;
let m = (module struct let x = 3 end);; (* Error *)
let m = (module struct let x = 3 end : S);;
f m 1 m;;
f m 1 (module struct let x = 2 end);;

let (module M) = m in M.x;;
let (module M) = m;; (* Error: only allowed in [let .. in] *)
class c = let (module M) = m in object end;; (* Error again *)
module M = (val m);;

module type S' = sig val f : int -> int end;;
(* Even works with recursion, but must be fully explicit *)
let rec (module M : S') =
  (module struct let f n = if n <= 0 then 1 else n * M.f (n-1) end : S')
in M.f 3;;

(* Subtyping *)

module type S = sig type t type u val x : t * u end
let f (l : (module S with type t = int and type u = bool) list) =
  (l :> (module S with type u = bool) list)

(* GADTs from the manual *)
(* the only modification is in to_string *)

module TypEq : sig
  type ('a, 'b) t
  val apply: ('a, 'b) t -> 'a -> 'b
  val refl: ('a, 'a) t
  val sym: ('a, 'b) t -> ('b, 'a) t
end = struct
  type ('a, 'b) t = ('a -> 'b) * ('b -> 'a)
  let refl = (fun x -> x), (fun x -> x)
  let apply (f, _) x = f x
  let sym (f, g) = (g, f)
end

module rec Typ : sig
  module type PAIR = sig
    type t and t1 and t2
    val eq: (t, t1 * t2) TypEq.t
    val t1: t1 Typ.typ
    val t2: t2 Typ.typ
  end

  type 'a typ =
    | Int of ('a, int) TypEq.t
    | String of ('a, string) TypEq.t
    | Pair of (module PAIR with type t = 'a)
end = Typ

let int = Typ.Int TypEq.refl

let str = Typ.String TypEq.refl

let pair (type s1) (type s2) t1 t2 =
  let module P = struct
    type t = s1 * s2
    type t1 = s1
    type t2 = s2
    let eq = TypEq.refl
    let t1 = t1
    let t2 = t2
  end in
  Typ.Pair (module P)

open Typ
let rec to_string: 'a. 'a Typ.typ -> 'a -> string =
  fun (type s) t x ->
    match (t : s typ) with
    | Int eq -> string_of_int (TypEq.apply eq x)
    | String eq -> Printf.sprintf "%S" (TypEq.apply eq x)
    | Pair (module P) ->
        let (x1, x2) = TypEq.apply P.eq x in
        Printf.sprintf "(%s,%s)" (to_string P.t1 x1) (to_string P.t2 x2)

(* Wrapping maps *)
module type MapT = sig
  include Map.S
  type data
  type map
  val of_t : data t -> map
  val to_t : map -> data t
end

type ('k,'d,'m) map =
    (module MapT with type key = 'k and type data = 'd and type map = 'm)

let add (type k) (type d) (type m) (m:(k,d,m) map) x y s =
   let module M =
     (val m:MapT with type key = k and type data = d and type map = m) in
   M.of_t (M.add x y (M.to_t s))

module SSMap = struct
  include Map.Make(String)
  type data = string
  type map = data t
  let of_t x = x
  let to_t x = x
end

let ssmap =
  (module SSMap:
   MapT with type key = string and type data = string and type map = SSMap.map)
;;

let ssmap =
  (module struct include SSMap end :
   MapT with type key = string and type data = string and type map = SSMap.map)
;;

let ssmap =
  (let module S = struct include SSMap end in (module S) :
  (module
   MapT with type key = string and type data = string and type map = SSMap.map))
;;

let ssmap =
  (module SSMap: MapT with type key = _ and type data = _ and type map = _)
;;

let ssmap : (_,_,_) map = (module SSMap);;

add ssmap;;
