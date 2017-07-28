module Exp =
  struct

    type _ t =
      | IntLit : int -> int t
      | BoolLit : bool -> bool t
      | Pair : 'a t * 'b t -> ('a * 'b) t
      | App : ('a -> 'b) t * 'a t -> 'b t
      | Abs : ('a -> 'b) -> ('a -> 'b) t


    let rec eval : type s . s t -> s =
      function
        | IntLit x -> x
        | BoolLit y -> y
        | Pair (x,y) ->
            (eval x,eval y)
        | App (f,a) ->
            (eval f) (eval a)
        | Abs f -> f

    let discern : type a. a t -> _ = function
        IntLit _ -> 1
      | BoolLit _ -> 2
      | Pair _ -> 3
      | App _ -> 4
      | Abs _ -> 5
  end
;;

module List =
  struct
    type zero
    type _ t =
      | Nil : zero t
      | Cons : 'a * 'b t -> ('a * 'b) t
    let head =
      function
        | Cons (a,b) -> a
    let tail =
      function
        | Cons (a,b) -> b
    let rec length : type a . a t -> int =
      function
        | Nil -> 0
        | Cons (a,b) -> length b
  end
;;

module Nonexhaustive =
  struct
    type 'a u =
      | C1 : int -> int u
      | C2 : bool -> bool u

    type 'a v =
      | C1 : int -> int v

    let unexhaustive : type s . s u -> s =
      function
        | C2 x -> x


    module M : sig type t type u end =
      struct
        type t = int
        type u = bool
      end
    type 'a t =
      | Foo : M.t -> M.t t
      | Bar : M.u -> M.u t
    let same_type : type s . s t * s t -> bool  =
      function
        | Foo _ , Foo _ -> true
        | Bar _, Bar _ -> true
  end
;;

module Exhaustive =
  struct
    type t = int
    type u = bool
    type 'a v =
      | Foo : t -> t v
      | Bar : u -> u v

    let same_type : type s . s v * s v -> bool  =
      function
        | Foo _ , Foo _ -> true
        | Bar _, Bar _ -> true
  end
;;

module PR6862 = struct
  class c (Some x) = object method x : int = x end
  type _ opt = Just : 'a -> 'a opt | Nothing : 'a opt
  class d (Just x) = object method x : int = x end
end;;

module Existential_escape =
  struct
    type _ t = C : int -> int t
    type u = D : 'a t -> u
    let eval (D x) = x
  end
;;

module Rectype =
  struct
    type (_,_) t = C : ('a,'a) t
    let f : type s. (s, s*s) t -> unit =
      fun C -> () (* here s = s*s! *)
  end
;;

module Or_patterns =
struct
      type _ t =
      | IntLit : int -> int t
      | BoolLit : bool -> bool t

    let rec eval : type s . s t -> unit =
      function
        | (IntLit _ | BoolLit _) -> ()

end
;;

module Polymorphic_variants =
  struct
      type _ t =
      | IntLit : int -> int t
      | BoolLit : bool -> bool t

    let rec eval : type s . [`A] * s t -> unit =
      function
        | `A, IntLit _ -> ()
        | `A, BoolLit _ -> ()
  end
;;

module Propagation = struct
  type _ t =
      IntLit : int -> int t
    | BoolLit : bool -> bool t

  let check : type s. s t -> s = function
    | IntLit n -> n
    | BoolLit b -> b

  let check : type s. s t -> s = fun x ->
    let r = match x with
    | IntLit n -> (n : s )
    | BoolLit b -> b
    in r
end
;;

module Normal_constrs = struct
  type a = A
  type b = B

  let f = function A -> 1 | B -> 2
end;;

module PR6849 = struct
  type 'a t = Foo : int t

  let f : int -> int = function
      Foo -> 5
end;;

type _ t = Int : int t ;;

let ky x y = ignore (x = y); x ;;

let test : type a. a t -> a =
  function Int -> ky (1 : a) 1
;;

let test : type a. a t -> _ =
  function Int -> 1       (* ok *)
;;

let test : type a. a t -> _ =
  function Int -> ky (1 : a) 1  (* fails *)
;;

let test : type a. a t -> a = fun x ->
  let r = match x with Int -> ky (1 : a) 1  (* fails *)
  in r
;;
let test : type a. a t -> a = fun x ->
  let r = match x with Int -> ky 1 (1 : a)  (* fails *)
  in r
;;
let test (type a) x =
  let r = match (x : a t) with Int -> ky 1 1
  in r
;;
let test : type a. a t -> a = fun x ->
  let r = match x with Int -> (1 : a)       (* ok! *)
  in r
;;
let test : type a. a t -> _ = fun x ->
  let r = match x with Int -> 1       (* ok! *)
  in r
;;
let test : type a. a t -> a = fun x ->
  let r : a = match x with Int -> 1
  in r (* ok *)
;;
let test2 : type a. a t -> a option = fun x ->
  let r = ref None in
  begin match x with Int -> r := Some (1 : a) end;
  !r (* ok *)
;;
let test2 : type a. a t -> a option = fun x ->
  let r : a option ref = ref None in
  begin match x with Int -> r := Some 1 end;
  !r (* ok *)
;;
let test2 : type a. a t -> a option = fun x ->
  let r : a option ref = ref None in
  let u = ref None in
  begin match x with Int -> r := Some 1; u := !r end;
  !u
;; (* ok (u non-ambiguous) *)
let test2 : type a. a t -> a option = fun x ->
  let r : a option ref = ref None in
  let u = ref None in
  begin match x with Int -> u := Some 1; r := !u end;
  !u
;; (* fails because u : (int | a) option ref *)
let test2 : type a. a t -> a option = fun x ->
  let u = ref None in
  let r : a option ref = ref None in
  begin match x with Int -> r := Some 1; u := !r end;
  !u
;; (* ok *)
let test2 : type a. a t -> a option = fun x ->
  let u = ref None in
  let a =
    let r : a option ref = ref None in
    begin match x with Int -> r := Some 1; u := !r end;
    !u
  in a
;; (* ok *)
let either = ky
let we_y1x (type a) (x : a) (v : a t) =
  match v with Int -> let y = either 1 x in y
;; (* fail *)

(* Effect of external consraints *)
let f (type a) (x : a t) y =
  ignore (y : a);
  let r = match x with Int -> (y : a) in (* ok *)
  r
;;
let f (type a) (x : a t) y =
  let r = match x with Int -> (y : a) in
  ignore (y : a); (* ok *)
  r
;;
let f (type a) (x : a t) y =
  ignore (y : a);
  let r = match x with Int -> y in (* ok *)
  r
;;
let f (type a) (x : a t) y =
  let r = match x with Int -> y in
  ignore (y : a); (* ok *)
  r
;;
let f (type a) (x : a t) (y : a) =
  match x with Int -> y (* returns 'a *)
;;

(* Combination with local modules *)

let f (type a) (x : a t) y =
  match x with Int ->
    let module M = struct type b = a let z = (y : b) end
    in M.z
;; (* fails because of aliasing... *)

let f (type a) (x : a t) y =
  match x with Int ->
    let module M = struct type b = int let z = (y : b) end
    in M.z
;; (* ok *)

(* Objects and variants *)

type _ h =
  | Has_m : <m : int> h
  | Has_b : <b : bool> h

let f : type a. a h -> a = function
  | Has_m -> object method m = 1 end
  | Has_b -> object method b = true end
;;
type _ j =
  | Has_A : [`A of int] j
  | Has_B : [`B of bool] j

let f : type a. a j -> a = function
  | Has_A -> `A 1
  | Has_B -> `B true
;;

type (_,_) eq = Eq : ('a,'a) eq ;;

let f : type a b. (a,b) eq -> (<m : a; ..> as 'c) -> (<m : b; ..> as 'c) =
  fun Eq o -> o
;; (* fail *)

let f : type a b. (a,b) eq -> <m : a; ..> -> <m : b; ..> =
  fun Eq o -> o
;; (* fail *)

let f (type a) (type b) (eq : (a,b) eq) (o : <m : a; ..>) : <m : b; ..> =
  match eq with Eq -> o ;; (* should fail *)

let f : type a b. (a,b) eq -> <m : a> -> <m : b> =
  fun Eq o -> o
;; (* ok *)

let int_of_bool : (bool,int) eq = Obj.magic Eq;;

let x = object method m = true end;;
let y = (x, f int_of_bool x);;

let f : type a. (a, int) eq -> <m : a> -> bool =
  fun Eq o -> ignore (o : <m : int; ..>); o#m = 3
;; (* should be ok *)

let f : type a b. (a,b) eq -> < m : a; .. > -> < m : b > =
  fun eq o ->
    ignore (o : < m : a >);
    let r : < m : b > = match eq with Eq -> o in (* fail with principal *)
    r;;

let f : type a b. (a,b) eq -> < m : a; .. > -> < m : b > =
  fun eq o ->
    let r : < m : b > = match eq with Eq -> o in (* fail *)
    ignore (o : < m : a >);
    r;;

let f : type a b. (a,b) eq -> [> `A of a] -> [> `A of b] =
  fun Eq o -> o ;; (* fail *)

let f (type a) (type b) (eq : (a,b) eq) (v : [> `A of a]) : [> `A of b] =
  match eq with Eq -> v ;; (* should fail *)

let f : type a b. (a,b) eq -> [< `A of a | `B] -> [< `A of b | `B] =
  fun Eq o -> o ;; (* fail *)

let f : type a b. (a,b) eq -> [`A of a | `B] -> [`A of b | `B] =
  fun Eq o -> o ;; (* ok *)

let f : type a. (a, int) eq -> [`A of a] -> bool =
  fun Eq v -> match v with `A 1 -> true | _ -> false
;; (* ok *)

let f : type a b. (a,b) eq -> [> `A of a | `B] -> [`A of b | `B] =
  fun eq o ->
    ignore (o : [< `A of a | `B]);
    let r : [`A of b | `B] = match eq with Eq -> o in (* fail with principal *)
    r;;

let f : type a b. (a,b) eq -> [> `A of a | `B] -> [`A of b | `B] =
  fun eq o ->
    let r : [`A of b | `B] = match eq with Eq -> o in (* fail *)
    ignore (o : [< `A of a | `B]);
    r;;

(* Pattern matching *)

type 'a t =
    A of int | B of bool | C of float | D of 'a

type _ ty =
  | TE : 'a ty -> 'a array ty
  | TA : int ty
  | TB : bool ty
  | TC : float ty
  | TD : string -> bool ty

let f : type a. a ty -> a t -> int = fun x y ->
  match x, y with
  | _, A z -> z
  | _, B z -> if z then 1 else 2
  | _, C z -> truncate z
  | TE TC, D [|1.0|] -> 14
  | TA, D 0 -> -1
  | TA, D z -> z
  | TD "bye", D false -> 13
  | TD "hello", D true -> 12
 (* | TB, D z -> if z then 1 else 2 *)
  | TC, D z -> truncate z
  | _, D _ -> 0
;;

let f : type a. a ty -> a t -> int = fun x y ->
  match x, y with
  | _, A z -> z
  | _, B z -> if z then 1 else 2
  | _, C z -> truncate z
  | TE TC, D [|1.0|] -> 14
  | TA, D 0 -> -1
  | TA, D z -> z
;; (* warn *)

let f : type a. a ty -> a t -> int = fun x y ->
  match y, x with
  | A z, _ -> z
  | B z, _ -> if z then 1 else 2
  | C z, _ -> truncate z
  | D [|1.0|], TE TC -> 14
  | D 0, TA -> -1
  | D z, TA -> z
;; (* fail *)

type ('a,'b) pair = {right:'a; left:'b}

let f : type a. a ty -> a t -> int = fun x y ->
  match {left=x; right=y} with
  | {left=_; right=A z} -> z
  | {left=_; right=B z} -> if z then 1 else 2
  | {left=_; right=C z} -> truncate z
  | {left=TE TC; right=D [|1.0|]} -> 14
  | {left=TA; right=D 0} -> -1
  | {left=TA; right=D z} -> z
;; (* fail *)

type ('a,'b) pair = {left:'a; right:'b}

let f : type a. a ty -> a t -> int = fun x y ->
  match {left=x; right=y} with
  | {left=_; right=A z} -> z
  | {left=_; right=B z} -> if z then 1 else 2
  | {left=_; right=C z} -> truncate z
  | {left=TE TC; right=D [|1.0|]} -> 14
  | {left=TA; right=D 0} -> -1
  | {left=TA; right=D z} -> z
;; (* ok *)

(* Injectivity *)

module M : sig type 'a t val eq : ('a t, 'b t) eq end =
  struct type 'a t = int let eq = Eq end
;;

let f : type a b. (a M.t, b M.t) eq -> (a, b) eq =
  function Eq -> Eq (* fail *)
;;

let f : type a b. (a M.t * a, b M.t * b) eq -> (a, b) eq =
  function Eq -> Eq (* ok *)
;;

let f : type a b. (a * a M.t, b * b M.t) eq -> (a, b) eq =
  function Eq -> Eq (* ok *)
;;

(* Applications of polymorphic variants *)

type _ t =
  | V1 : [`A | `B] t
  | V2 : [`C | `D] t

let f : type a. a t -> a = function
  | V1 -> `A
  | V2 -> `C
;;

f V1;;

(* PR#5425 and PR#5427 *)

type _ int_foo =
  | IF_constr : <foo:int; ..> int_foo

type _ int_bar =
  | IB_constr : <bar:int; ..> int_bar
;;

let g (type t) (x:t) (e : t int_foo) (e' : t int_bar) =
  let IF_constr, IB_constr = e, e' in
  (x:<foo:int>)
;;

let g (type t) (x:t) (e : t int_foo) (e' : t int_bar) =
  let IF_constr, IB_constr = e, e' in
  (x:<foo:int;bar:int>)
;;

let g (type t) (x:t) (e : t int_foo) (e' : t int_bar) =
  let IF_constr, IB_constr = e, e' in
  (x:<foo:int;bar:int;..>)
;;

let g (type t) (x:t) (e : t int_foo) (e' : t int_bar) : t =
  let IF_constr, IB_constr = e, e' in
  (x:<foo:int;bar:int;..>)
;;

let g (type t) (x:t) (e : t int_foo) (e' : t int_bar) =
  let IF_constr, IB_constr = e, e' in
  x, x#foo, x#bar
;;

(* PR#5554 *)

type 'a ty = Int : int -> int ty;;

let f : type a. a ty -> a =
  fun x -> match x with Int y -> y;;

let g : type a. a ty -> a =
  let () = () in
  fun x -> match x with Int y -> y;;

(* Printing of anonymous variables *)

module M = struct type _ t = int end;;
module M = struct type _ t = T : int t end;;
module N = M;;

(* Principality *)

(* adding a useless equation should not break inference *)
let f : type a b. (a,b) eq -> (a,int) eq -> a -> b -> _ = fun ab aint a b ->
  let Eq = ab in
  let x =
    let Eq = aint in
    if true then a else b
  in ignore x
;; (* ok *)

let f : type a b. (a,b) eq -> (b,int) eq -> a -> b -> _ = fun ab bint a b ->
  let Eq = ab in
  let x =
    let Eq = bint in
    if true then a else b
  in ignore x
;; (* ok *)
