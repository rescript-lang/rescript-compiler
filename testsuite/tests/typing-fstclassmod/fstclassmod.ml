(* Example of algorithm parametrized with modules *)

let sort (type s) set l =
  let module Set = (val set : Set.S with type elt = s) in
  Set.elements (List.fold_right Set.add l Set.empty)

let make_set (type s) cmp =
  let module S = Set.Make(struct
    type t = s
    let compare = cmp
  end) in
  (module S : Set.S with type elt = s)

let both l =
  List.map
    (fun set -> sort set l)
    [ make_set compare; make_set (fun x y -> compare y x) ]

let () =
  print_endline (String.concat "  " (List.map (String.concat "/") (both ["abc";"xyz";"def"])))


(* Hiding the internal representation *)

module type S = sig
  type t
  val to_string: t -> string
  val apply: t -> t
  val x: t
end

let create (type s) to_string apply x =
  let module M = struct
    type t = s
    let to_string = to_string
    let apply = apply
    let x = x
  end in
  (module M : S with type t = s)

let forget (type s) x =
  let module M = (val x : S with type t = s) in
  (module M : S)

let print x =
  let module M = (val x : S) in
  print_endline (M.to_string M.x)

let apply x =
  let module M = (val x : S) in
  let module N = struct
    include M
    let x = apply x
  end in
  (module N : S)

let () =
  let int = forget (create string_of_int succ 0) in
  let str = forget (create (fun s -> s) (fun s -> s ^ s) "X") in
  List.iter print (List.map apply [int; apply int; apply (apply str)])


(* Existential types + type equality witnesses -> pseudo GADT *)

module TypEq : sig
  type ('a, 'b) t
  val apply: ('a, 'b) t -> 'a -> 'b
  val refl: ('a, 'a) t
  val sym: ('a, 'b) t -> ('b, 'a) t
end = struct
  type ('a, 'b) t = unit
  let apply _ = Obj.magic
  let refl = ()
  let sym () = ()
end


module rec Typ : sig
  module type PAIR = sig
    type t
    type t1
    type t2
    val eq: (t, t1 * t2) TypEq.t
    val t1: t1 Typ.typ
    val t2: t2 Typ.typ
  end

  type 'a typ =
    | Int of ('a, int) TypEq.t
    | String of ('a, string) TypEq.t
    | Pair of (module PAIR with type t = 'a)
end = struct
  module type PAIR = sig
    type t
    type t1
    type t2
    val eq: (t, t1 * t2) TypEq.t
    val t1: t1 Typ.typ
    val t2: t2 Typ.typ
  end

  type 'a typ =
    | Int of ('a, int) TypEq.t
    | String of ('a, string) TypEq.t
    | Pair of (module PAIR with type t = 'a)
end

open Typ

let int = Int TypEq.refl

let str = String TypEq.refl

let pair (type s1) (type s2) t1 t2 =
  let module P = struct
    type t = s1 * s2
    type t1 = s1
    type t2 = s2
    let eq = TypEq.refl
    let t1 = t1
    let t2 = t2
  end in
  let pair = (module P : PAIR with type t = s1 * s2) in
  Pair pair

module rec Print : sig
  val to_string: 'a Typ.typ -> 'a -> string
end = struct
  let to_string (type s) t x =
    match t with
    | Int eq -> string_of_int (TypEq.apply eq x)
    | String eq -> Printf.sprintf "%S" (TypEq.apply eq x)
    | Pair p ->
        let module P = (val p : PAIR with type t = s) in
        let (x1, x2) = TypEq.apply P.eq x in
        Printf.sprintf "(%s,%s)" (Print.to_string P.t1 x1) (Print.to_string P.t2 x2)
end

let () =
  print_endline (Print.to_string int 10);
  print_endline (Print.to_string (pair int (pair str int)) (123, ("A", 456)))


(* #6262: first-class modules and module type aliases *)

module type S1 = sig end
module type S2 = S1

let _f (x : (module S1)) : (module S2) = x

module X = struct
  module type S
end
module Y = struct include X end

let _f (x : (module X.S)) : (module Y.S) = x

(* PR#6194, main example *)
module type S3 = sig val x : bool end;;
let f = function
  | Some (module M : S3) when M.x ->1
  | Some _ -> 2
  | None -> 3
;;
print_endline (string_of_int (f (Some (module struct let x = false end))));;
