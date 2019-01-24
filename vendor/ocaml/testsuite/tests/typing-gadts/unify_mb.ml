(* First-Order Unification by Structural Recursion *)
(* Conor McBride, JFP 13(6) *)
(* http://strictlypositive.org/publications.html *)

(* This is a translation of the code part to ocaml *)
(* Of course, we do not prove other properties, not even termination *)

(* 2.2 Inductive Families *)

type zero = Zero
type _ succ = Succ
type _ nat =
  | NZ : zero nat
  | NS : 'a nat -> 'a succ nat

type _ fin =
  | FZ : 'a succ fin
  | FS : 'a fin -> 'a succ fin

(* We cannot define
     val empty : zero fin -> 'a
   because we cannot write an empty pattern matching.
   This might be useful to have *)

(* In place, prove that the parameter is 'a succ *)
type _ is_succ = IS : 'a succ is_succ

let fin_succ : type n. n fin -> n is_succ = function
  | FZ -> IS
  | FS _ -> IS
;;
[%%expect{|
type zero = Zero
type _ succ = Succ
type _ nat = NZ : zero nat | NS : 'a nat -> 'a succ nat
type _ fin = FZ : 'a succ fin | FS : 'a fin -> 'a succ fin
type _ is_succ = IS : 'a succ is_succ
val fin_succ : 'n fin -> 'n is_succ = <fun>
|}];;

(* 3 First-Order Terms, Renaming and Substitution *)

type 'a term =
  | Var of 'a fin
  | Leaf
  | Fork of 'a term * 'a term

let var x = Var x

let lift r : 'm fin -> 'n term = fun x -> Var (r x)

let rec pre_subst f = function
  | Var x -> f x
  | Leaf -> Leaf
  | Fork (t1, t2) -> Fork (pre_subst f t1, pre_subst f t2)

let comp_subst f g (x : 'a fin) = pre_subst f (g x)
(*  val comp_subst :
    ('b fin -> 'c term) -> ('a fin -> 'b term) -> 'a fin -> 'c term *)
;;
[%%expect{|
type 'a term = Var of 'a fin | Leaf | Fork of 'a term * 'a term
val var : 'a fin -> 'a term = <fun>
val lift : ('m fin -> 'n fin) -> 'm fin -> 'n term = <fun>
val pre_subst : ('a fin -> 'b term) -> 'a term -> 'b term = <fun>
val comp_subst :
  ('b fin -> 'c term) -> ('a fin -> 'b term) -> 'a fin -> 'c term = <fun>
|}];;

(* 4 The Occur-Check, through thick and thin *)

let rec thin : type n. n succ fin -> n fin -> n succ fin =
  fun x y -> match x, y with
  | FZ, y    -> FS y
  | FS x, FZ -> FZ
  | FS x, FS y -> FS (thin x y)
[%%expect{|
val thin : 'n succ fin -> 'n fin -> 'n succ fin = <fun>
|}];;

let bind t f =
  match t with
  | None   -> None
  | Some x -> f x
(* val bind : 'a option -> ('a -> 'b option) -> 'b option *)
[%%expect{|
val bind : 'a option -> ('a -> 'b option) -> 'b option = <fun>
|}];;

let rec thick : type n. n succ fin -> n succ fin -> n fin option =
  fun x y -> match x, y with
  | FZ, FZ   -> None
  | FZ, FS y -> Some y
  | FS x, FZ -> let IS = fin_succ x in Some FZ
  | FS x, FS y ->
      let IS = fin_succ x in bind (thick x y) (fun x -> Some (FS x))
[%%expect{|
val thick : 'n succ fin -> 'n succ fin -> 'n fin option = <fun>
|}];;

let rec check : type n. n succ fin -> n succ term -> n term option =
  fun x t -> match t with
  | Var y -> bind (thick x y) (fun x -> Some (Var x))
  | Leaf  -> Some Leaf
  | Fork (t1, t2) ->
      bind (check x t1) (fun t1 ->
        bind (check x t2) (fun t2 -> Some (Fork (t1, t2))))
[%%expect{|
val check : 'n succ fin -> 'n succ term -> 'n term option = <fun>
|}];;

let subst_var x t' y =
  match thick x y with
  | None -> t'
  | Some y' -> Var y'
(* val subst_var : 'a succ fin -> 'a term -> 'a succ fin -> 'a term *)
[%%expect{|
val subst_var : 'a succ fin -> 'a term -> 'a succ fin -> 'a term = <fun>
|}];;

let subst x t' = pre_subst (subst_var x t')
(* val subst : 'a succ fin -> 'a term -> 'a succ term -> 'a term *)
;;
[%%expect{|
val subst : 'a succ fin -> 'a term -> 'a succ term -> 'a term = <fun>
|}];;

(* 5 A Refinement of Substitution *)

type (_,_) alist =
  | Anil  : ('n,'n) alist
  | Asnoc : ('m,'n) alist * 'm term * 'm succ fin -> ('m succ, 'n) alist

let rec sub : type m n. (m,n) alist -> m fin -> n term = function
  | Anil -> var
  | Asnoc (s, t, x) -> comp_subst (sub s) (subst_var x t)
[%%expect{|
type (_, _) alist =
    Anil : ('n, 'n) alist
  | Asnoc : ('m, 'n) alist * 'm term * 'm succ fin -> ('m succ, 'n) alist
val sub : ('m, 'n) alist -> 'm fin -> 'n term = <fun>
|}];;

let rec append : type m n l. (m,n) alist -> (l,m) alist -> (l,n) alist =
  fun r s -> match s with
  | Anil -> r
  | Asnoc (s, t, x) -> Asnoc (append r s, t, x)
[%%expect{|
val append : ('m, 'n) alist -> ('l, 'm) alist -> ('l, 'n) alist = <fun>
|}];;

type _ ealist = EAlist : ('a,'b) alist -> 'a ealist

let asnoc a t' x = EAlist (Asnoc (a, t', x))
[%%expect{|
type _ ealist = EAlist : ('a, 'b) alist -> 'a ealist
val asnoc : ('a, 'b) alist -> 'a term -> 'a succ fin -> 'a succ ealist =
  <fun>
|}];;

(* Extra work: we need sub to work on ealist too, for examples *)
let rec weaken_fin : type n. n fin -> n succ fin = function
  | FZ -> FZ
  | FS x -> FS (weaken_fin x)

let weaken_term t = pre_subst (fun x -> Var (weaken_fin x)) t

let rec weaken_alist : type m n. (m, n) alist -> (m succ, n succ) alist =
  function
    | Anil -> Anil
    | Asnoc (s, t, x) -> Asnoc (weaken_alist s, weaken_term t, weaken_fin x)

let rec sub' : type m. m ealist -> m fin -> m term = function
  | EAlist Anil -> var
  | EAlist (Asnoc (s, t, x)) ->
      comp_subst (sub' (EAlist (weaken_alist s)))
        (fun t' -> weaken_term (subst_var x t t'))

let subst' d = pre_subst (sub' d)
(*  val subst' : 'a ealist -> 'a term -> 'a term *)
;;
[%%expect{|
val weaken_fin : 'n fin -> 'n succ fin = <fun>
val weaken_term : 'a term -> 'a succ term = <fun>
val weaken_alist : ('m, 'n) alist -> ('m succ, 'n succ) alist = <fun>
val sub' : 'm ealist -> 'm fin -> 'm term = <fun>
val subst' : 'a ealist -> 'a term -> 'a term = <fun>
|}];;

(* 6 First-Order Unification *)

let flex_flex x y =
  match thick x y with
  | Some y' -> asnoc Anil (Var y') x
  | None -> EAlist Anil
(* val flex_flex : 'a succ fin -> 'a succ fin -> 'a succ ealist *)

let flex_rigid x t =
  bind (check x t) (fun t' -> Some (asnoc Anil t' x))
(* val flex_rigid : 'a succ fin -> 'a succ term -> 'a succ ealist option *)

let rec amgu : type m. m term -> m term -> m ealist -> m ealist option =
  fun s t acc -> match s, t, acc with
  | Leaf, Leaf, _   -> Some acc
  | Leaf, Fork _, _ -> None
  | Fork _, Leaf, _ -> None
  | Fork (s1, s2), Fork (t1, t2), _ ->
      bind (amgu s1 t1 acc) (amgu s2 t2)
  | Var x, Var y, EAlist Anil -> let IS = fin_succ x in Some (flex_flex x y)
  | Var x, t,     EAlist Anil -> let IS = fin_succ x in flex_rigid x t
  | t, Var x,     EAlist Anil -> let IS = fin_succ x in flex_rigid x t
  | s, t, EAlist(Asnoc(d,r,z)) ->
      bind (amgu (subst z r s) (subst z r t) (EAlist d))
           (fun (EAlist d) -> Some (asnoc d r z))

let mgu s t = amgu s t (EAlist Anil)
(* val mgu : 'a term -> 'a term -> 'a ealist option *)
;;
[%%expect{|
val flex_flex : 'a succ fin -> 'a succ fin -> 'a succ ealist = <fun>
val flex_rigid : 'a succ fin -> 'a succ term -> 'a succ ealist option = <fun>
val amgu : 'm term -> 'm term -> 'm ealist -> 'm ealist option = <fun>
val mgu : 'a term -> 'a term -> 'a ealist option = <fun>
|}];;

let s = Fork (Var FZ, Fork (Var (FS (FS FZ)), Leaf))
let t = Fork (Var (FS FZ), Var (FS FZ))
let d = match mgu s t with Some x -> x | None -> failwith "mgu"
let s' = subst' d s
let t' = subst' d t
;;
[%%expect{|
val s : 'a succ succ succ term = Fork (Var FZ, Fork (Var (FS (FS FZ)), Leaf))
val t : 'a succ succ term = Fork (Var (FS FZ), Var (FS FZ))
val d : '_weak1 succ succ succ ealist =
  EAlist (Asnoc (Asnoc (Anil, Fork (Var FZ, Leaf), FZ), Var FZ, FZ))
val s' : '_weak1 succ succ succ term =
  Fork (Fork (Var FZ, Leaf), Fork (Var FZ, Leaf))
val t' : '_weak1 succ succ succ term =
  Fork (Fork (Var FZ, Leaf), Fork (Var FZ, Leaf))
|}];;
