(*
   An attempt at encoding omega examples from the 2nd Central European
   Functional Programming School:
     Generic Programming in Omega, by Tim Sheard and Nathan Linger
          http://web.cecs.pdx.edu/~sheard/
*)

(* Basic types *)

type ('a,'b) sum = Inl of 'a | Inr of 'b

type zero = Zero
type 'a succ = Succ of 'a
type _ nat =
  | NZ : zero nat
  | NS : 'a nat -> 'a succ nat
;;

(* 2: A simple example *)

type (_,_) seq =
  | Snil  : ('a,zero) seq
  | Scons : 'a * ('a,'n) seq -> ('a, 'n succ) seq
;;

let l1 = Scons (3, Scons (5, Snil)) ;;

(* We do not have type level functions, so we need to use witnesses. *)
(* We copy here the definitions from section 3.9 *)
(* Note the addition of the ['a nat] argument to PlusZ, since we do not
   have kinds *)
type (_,_,_) plus =
  | PlusZ : 'a nat -> (zero, 'a, 'a) plus
  | PlusS : ('a,'b,'c) plus -> ('a succ, 'b, 'c succ) plus
;;

let rec length : type a n. (a,n) seq -> n nat = function
  | Snil -> NZ
  | Scons (_, s) -> NS (length s)
;;

(* app returns the catenated lists with a witness proving that
   the size is the sum of its two inputs *)
type (_,_,_) app = App : ('a,'p) seq * ('n,'m,'p) plus -> ('a,'n,'m) app

let rec app : type a n m. (a,n) seq -> (a,m) seq -> (a,n,m) app =
  fun xs ys ->
  match xs with
  | Snil -> App (ys, PlusZ (length ys))
  | Scons (x, xs') ->
      let App (xs'', pl) = app xs' ys in
      App (Scons (x, xs''), PlusS pl)
;;

(* 3.1 Feature: kinds *)

(* We do not have kinds, but we can encode them as predicates *)

type tp = TP
type nd = ND
type ('a,'b) fk = FK
type _ shape =
  | Tp : tp shape
  | Nd : nd shape
  | Fk : 'a shape * 'b shape -> ('a,'b) fk shape
;;
type tt = TT
type ff = FF
type _ boolean =
  | BT : tt boolean
  | BF : ff boolean
;;

(* 3.3 Feature : GADTs *)

type (_,_) path =
  | Pnone : 'a -> (tp,'a) path
  | Phere : (nd,'a) path
  | Pleft : ('x,'a) path -> (('x,'y) fk, 'a) path
  | Pright : ('y,'a) path -> (('x,'y) fk, 'a) path
;;
type (_,_) tree =
  | Ttip  : (tp,'a) tree
  | Tnode : 'a -> (nd,'a) tree
  | Tfork : ('x,'a) tree * ('y,'a) tree -> (('x,'y)fk, 'a) tree
;;
let tree1 = Tfork (Tfork (Ttip, Tnode 4), Tfork (Tnode 4, Tnode 3))
;;
let rec find : type sh.
    ('a -> 'a -> bool) -> 'a -> (sh,'a) tree -> (sh,'a) path list
  = fun eq n t ->
    match t with
    | Ttip -> []
    | Tnode m ->
        if eq n m then [Phere] else []
    | Tfork (x, y) ->
        List.map (fun x -> Pleft x) (find eq n x) @
        List.map (fun x -> Pright x) (find eq n y)
;;
let rec extract : type sh. (sh,'a) path -> (sh,'a) tree -> 'a = fun p t ->
  match (p, t) with
  | Pnone x, Ttip -> x
  | Phere, Tnode y -> y
  | Pleft p, Tfork(l,_) -> extract p l
  | Pright p, Tfork(_,r) -> extract p r
;;

(* 3.4 Pattern : Witness *)

type (_,_) le =
  | LeZ : 'a nat -> (zero, 'a) le
  | LeS : ('n, 'm) le -> ('n succ, 'm succ) le
;;
type _ even =
  | EvenZ : zero even
  | EvenSS : 'n even -> 'n succ succ even
;;
type one = zero succ
type two = one succ
type three = two succ
type four = three succ
;;
let even0 : zero even = EvenZ
let even2 : two even = EvenSS EvenZ
let even4 : four even = EvenSS (EvenSS EvenZ)
;;
let p1 : (two, one, three) plus = PlusS (PlusS (PlusZ (NS NZ)))
;;
let rec summandLessThanSum : type a b c. (a,b,c) plus -> (a,c) le = fun p ->
  match p with
  | PlusZ n -> LeZ n
  | PlusS p' -> LeS (summandLessThanSum p')
;;

(* 3.8 Pattern: Leibniz Equality *)

type (_,_) equal = Eq : ('a,'a) equal

let convert : type a b. (a,b) equal -> a -> b = fun Eq x -> x

let rec sameNat : type a b. a nat -> b nat -> (a,b) equal option = fun a b ->
  match a, b with
  | NZ, NZ -> Some Eq
  | NS a', NS b' ->
      begin match sameNat a' b' with
      | Some Eq -> Some Eq
      | None -> None
      end
  | _ -> None
;;

(* Extra: associativity of addition *)

let rec plus_func : type a b m n.
  (a,b,m) plus -> (a,b,n) plus -> (m,n) equal =
  fun p1 p2 ->
  match p1, p2 with
  | PlusZ _, PlusZ _ -> Eq
  | PlusS p1', PlusS p2' ->
      let Eq = plus_func p1' p2' in Eq

let rec plus_assoc : type a b c ab bc m n.
  (a,b,ab) plus -> (ab,c,m) plus ->
  (b,c,bc) plus -> (a,bc,n) plus -> (m,n) equal = fun p1 p2 p3 p4 ->
  match p1, p4 with
  | PlusZ b, PlusZ bc ->
      let Eq = plus_func p2 p3 in Eq
  | PlusS p1', PlusS p4' ->
      let PlusS p2' = p2 in
      let Eq = plus_assoc p1' p2' p3 p4' in Eq
;;

(* 3.9 Computing Programs and Properties Simultaneously *)

(* Plus and app1 are moved to section 2 *)

let smaller : type a b. (a succ, b succ) le -> (a,b) le =
  function LeS x -> x ;;

type (_,_) diff = Diff : 'c nat * ('a,'c,'b) plus -> ('a,'b) diff ;;

(*
let rec diff : type a b. (a,b) le -> a nat -> b nat -> (a,b) diff =
  fun le a b ->
  match a, b, le with
  | NZ, m, _ -> Diff (m, PlusZ m)
  | NS x, NZ, _ -> assert false
  | NS x, NS y, q ->
      match diff (smaller q) x y with Diff (m, p) -> Diff (m, PlusS p)
;;
*)

let rec diff : type a b. (a,b) le -> a nat -> b nat -> (a,b) diff =
  fun le a b ->
  match le, a, b with
  | LeZ _, _, m -> Diff (m, PlusZ m)
  | LeS q, NS x, NS y ->
      match diff q x y with Diff (m, p) -> Diff (m, PlusS p)
;;

let rec diff : type a b. (a,b) le -> a nat -> b nat -> (a,b) diff =
  fun le a b ->
  match a, b,le with (* warning *)
  | NZ, m, LeZ _ -> Diff (m, PlusZ m)
  | NS x, NS y, LeS q ->
      match diff q x y with Diff (m, p) -> Diff (m, PlusS p)
;;

let rec diff : type a b. (a,b) le -> b nat -> (a,b) diff =
  fun le b ->
  match b,le with
  | m, LeZ _ -> Diff (m, PlusZ m)
  | NS y, LeS q ->
      match diff q y with Diff (m, p) -> Diff (m, PlusS p)
;;

type (_,_) filter = Filter : ('m,'n) le * ('a,'m) seq -> ('a,'n) filter

let rec leS' : type m n. (m,n) le -> (m,n succ) le = function
  | LeZ n -> LeZ (NS n)
  | LeS le -> LeS (leS' le)
;;

let rec filter : type a n. (a -> bool) -> (a,n) seq -> (a,n) filter =
  fun f s ->
  match s with
  | Snil -> Filter (LeZ NZ, Snil)
  | Scons (a,l) ->
      match filter f l with Filter (le, l') ->
        if f a then Filter (LeS le, Scons (a, l'))
        else Filter (leS' le, l')
;;

(* 4.1 AVL trees *)

type (_,_,_) balance =
  | Less : ('h, 'h succ, 'h succ) balance
  | Same : ('h, 'h, 'h) balance
  | More : ('h succ, 'h, 'h succ) balance

type _ avl =
  | Leaf : zero avl
  | Node :
      ('hL, 'hR, 'hMax) balance * 'hL avl * int * 'hR avl -> 'hMax succ avl

type avl' = Avl : 'h avl -> avl'
;;

let empty = Avl Leaf

let rec elem : type h. int -> h avl -> bool = fun x t ->
  match t with
  | Leaf -> false
  | Node (_, l, y, r) ->
      x = y || if x < y then elem x l else elem x r
;;

let rec rotr : type n. (n succ succ) avl -> int -> n avl ->
  ((n succ succ) avl, (n succ succ succ) avl) sum =
  fun tL y tR ->
  match tL with
  | Node (Same, a, x, b) -> Inr (Node (Less, a, x, Node (More, b, y, tR)))
  | Node (More, a, x, b) -> Inl (Node (Same, a, x, Node (Same, b, y, tR)))
  | Node (Less, a, x, Node (Same, b, z, c)) ->
      Inl (Node (Same, Node (Same, a, x, b), z, Node (Same, c, y, tR)))
  | Node (Less, a, x, Node (Less, b, z, c)) ->
      Inl (Node (Same, Node (More, a, x, b), z, Node (Same, c, y, tR)))
  | Node (Less, a, x, Node (More, b, z, c)) ->
      Inl (Node (Same, Node (Same, a, x, b), z, Node (Less, c, y, tR)))
;;
let rec rotl : type n. n avl -> int -> (n succ succ) avl ->
  ((n succ succ) avl, (n succ succ succ) avl) sum =
  fun tL u tR ->
  match tR with
  | Node (Same, a, x, b) -> Inr (Node (More, Node (Less, tL, u, a), x, b))
  | Node (Less, a, x, b) -> Inl (Node (Same, Node (Same, tL, u, a), x, b))
  | Node (More, Node (Same, a, x, b), y, c) ->
      Inl (Node (Same, Node (Same, tL, u, a), x, Node (Same, b, y, c)))
  | Node (More, Node (Less, a, x, b), y, c) ->
      Inl (Node (Same, Node (More, tL, u, a), x, Node (Same, b, y, c)))
  | Node (More, Node (More, a, x, b), y, c) ->
      Inl (Node (Same, Node (Same, tL, u, a), x, Node (Less, b, y, c)))
;;
let rec ins : type n. int -> n avl -> (n avl, (n succ) avl) sum =
  fun x t ->
  match t with
  | Leaf -> Inr (Node (Same, Leaf, x, Leaf))
  | Node (bal, a, y, b) ->
      if x = y then Inl t else
      if x < y then begin
        match ins x a with
        | Inl a -> Inl (Node (bal, a, y, b))
        | Inr a ->
            match bal with
            | Less -> Inl (Node (Same, a, y, b))
            | Same -> Inr (Node (More, a, y, b))
            | More -> rotr a y b
      end else begin
        match ins x b with
        | Inl b -> Inl (Node (bal, a, y, b) : n avl)
        | Inr b ->
            match bal with
            | More -> Inl (Node (Same, a, y, b) : n avl)
            | Same -> Inr (Node (Less, a, y, b) : n succ avl)
            | Less -> rotl a y b
      end
;;

let insert x (Avl t) =
  match ins x t with
  | Inl t -> Avl t
  | Inr t -> Avl t
;;

let rec del_min : type n. (n succ) avl -> int * (n avl, (n succ) avl) sum =
  function
  | Node (Less, Leaf, x, r) -> (x, Inl r)
  | Node (Same, Leaf, x, r) -> (x, Inl r)
  | Node (bal, (Node _ as l) , x, r) ->
      match del_min l with
      | y, Inr l -> (y, Inr (Node (bal, l, x, r)))
      | y, Inl l ->
          (y, match bal with
          | Same -> Inr (Node (Less, l, x, r))
          | More -> Inl (Node (Same, l, x, r))
          | Less -> rotl l x r)

type _ avl_del =
  | Dsame : 'n avl -> 'n avl_del
  | Ddecr : ('m succ, 'n) equal * 'm avl -> 'n avl_del

let rec del : type n. int -> n avl -> n avl_del = fun y t ->
  match t with
  | Leaf -> Dsame Leaf
  | Node (bal, l, x, r) ->
      if x = y then begin
        match r with
        | Leaf ->
            begin match bal with
            | Same -> Ddecr (Eq, l)
            | More -> Ddecr (Eq, l)
            end
        | Node _ ->
            begin match bal, del_min r with
            | _, (z, Inr r) -> Dsame (Node (bal, l, z, r))
            | Same, (z, Inl r) -> Dsame (Node (More, l, z, r))
            | Less, (z, Inl r) -> Ddecr (Eq, Node (Same, l, z, r))
            | More, (z, Inl r) ->
                match rotr l z r with
                | Inl t -> Ddecr (Eq, t)
                | Inr t -> Dsame t
            end
      end else if y < x then begin
        match del y l with
        | Dsame l -> Dsame (Node (bal, l, x, r))
        | Ddecr(Eq,l) ->
            begin match bal with
            | Same -> Dsame (Node (Less, l, x, r))
            | More -> Ddecr (Eq, Node (Same, l, x, r))
            | Less ->
                match rotl l x r with
                | Inl t -> Ddecr (Eq, t)
                | Inr t -> Dsame t
            end
      end else begin
        match del y r with
        | Dsame r -> Dsame (Node (bal, l, x, r))
        | Ddecr(Eq,r) ->
            begin match bal with
            | Same -> Dsame (Node (More, l, x, r))
            | Less -> Ddecr (Eq, Node (Same, l, x, r))
            | More ->
                match rotr l x r with
                | Inl t -> Ddecr (Eq, t)
                | Inr t -> Dsame t
            end
      end
;;

let delete x (Avl t) =
  match del x t with
  | Dsame t -> Avl t
  | Ddecr (_, t) -> Avl t
;;


(* Exercise 22: Red-black trees *)

type red = RED
type black = BLACK
type (_,_) sub_tree =
  | Bleaf : (black, zero) sub_tree
  | Rnode :
      (black, 'n) sub_tree * int * (black, 'n) sub_tree -> (red, 'n) sub_tree
  | Bnode :
      ('cL, 'n) sub_tree * int * ('cR, 'n) sub_tree -> (black, 'n succ) sub_tree

type rb_tree = Root : (black, 'n) sub_tree -> rb_tree
;;

type dir = LeftD | RightD

type (_,_) ctxt =
  | CNil : (black,'n) ctxt
  | CRed : int * dir * (black,'n) sub_tree * (red,'n) ctxt -> (black,'n) ctxt
  | CBlk : int * dir * ('c1,'n) sub_tree * (black, 'n succ) ctxt -> ('c,'n) ctxt
;;

let blacken = function
    Rnode (l, e, r) -> Bnode (l, e, r)

type _ crep =
  | Red : red crep
  | Black : black crep

let color : type c n. (c,n) sub_tree -> c crep = function
  | Bleaf -> Black
  | Rnode _ -> Red
  | Bnode _ -> Black
;;

let rec fill : type c n. (c,n) ctxt -> (c,n) sub_tree -> rb_tree =
  fun ct t ->
  match ct with
  | CNil -> Root t
  | CRed (e, LeftD, uncle, c) -> fill c (Rnode (uncle, e, t))
  | CRed (e, RightD, uncle, c) -> fill c (Rnode (t, e, uncle))
  | CBlk (e, LeftD, uncle, c) -> fill c (Bnode (uncle, e, t))
  | CBlk (e, RightD, uncle, c) -> fill c (Bnode (t, e, uncle))
;;
let recolor d1 pE sib d2 gE uncle t =
  match d1, d2 with
  | LeftD, RightD -> Rnode (Bnode (sib, pE, t), gE, uncle)
  | RightD, RightD -> Rnode (Bnode (t, pE, sib), gE, uncle)
  | LeftD, LeftD -> Rnode (uncle, gE, Bnode (sib, pE, t))
  | RightD, LeftD -> Rnode (uncle, gE, Bnode (t, pE, sib))
;;
let rotate d1 pE sib d2 gE uncle (Rnode (x, e, y)) =
  match d1, d2 with
  | RightD, RightD -> Bnode (Rnode (x,e,y), pE, Rnode (sib, gE, uncle))
  | LeftD,  RightD -> Bnode (Rnode (sib, pE, x), e, Rnode (y, gE, uncle))
  | LeftD,  LeftD  -> Bnode (Rnode (uncle, gE, sib), pE, Rnode (x,e,y))
  | RightD, LeftD  -> Bnode (Rnode (uncle, gE, x), e, Rnode (y, pE, sib))
;;
let rec repair : type c n. (red,n) sub_tree -> (c,n) ctxt -> rb_tree =
  fun t ct ->
  match ct with
  | CNil -> Root (blacken t)
  | CBlk (e, LeftD, sib, c) -> fill c (Bnode (sib, e, t))
  | CBlk (e, RightD, sib, c) -> fill c (Bnode (t, e, sib))
  | CRed (e, dir, sib, CBlk (e', dir', uncle, ct)) ->
      match color uncle with
      | Red -> repair (recolor dir e sib dir' e' (blacken uncle) t) ct
      | Black -> fill ct (rotate dir e sib dir' e' uncle t)
;;
let rec ins : type c n. int -> (c,n) sub_tree -> (c,n) ctxt -> rb_tree =
  fun e t ct ->
  match t with
  | Rnode (l, e', r) ->
      if e < e' then ins e l (CRed (e', RightD, r, ct))
                else ins e r (CRed (e', LeftD, l, ct))
  | Bnode (l, e', r) ->
      if e < e' then ins e l (CBlk (e', RightD, r, ct))
                else ins e r (CBlk (e', LeftD, l, ct))
  | Bleaf -> repair (Rnode (Bleaf, e, Bleaf)) ct
;;
let insert e (Root t) = ins e t CNil
;;

(* 5.7 typed object languages using GADTs *)

type _ term =
  | Const : int -> int term
  | Add   : (int * int -> int) term
  | LT    : (int * int -> bool) term
  | Ap    : ('a -> 'b) term * 'a term -> 'b term
  | Pair  : 'a term * 'b term -> ('a * 'b) term

let ex1 = Ap (Add, Pair (Const 3, Const 5))
let ex2 = Pair (ex1, Const 1)

let rec eval_term : type a. a term -> a = function
  | Const x -> x
  | Add -> fun (x,y) -> x+y
  | LT  -> fun (x,y) -> x<y
  | Ap(f,x) -> eval_term f (eval_term x)
  | Pair(x,y) -> (eval_term x, eval_term y)

type _ rep =
  | Rint  : int rep
  | Rbool : bool rep
  | Rpair : 'a rep * 'b rep -> ('a * 'b) rep
  | Rfun  : 'a rep * 'b rep -> ('a -> 'b) rep

type (_,_) equal = Eq : ('a,'a) equal

let rec rep_equal : type a b. a rep -> b rep -> (a, b) equal option =
  fun ra rb ->
  match ra, rb with
  | Rint, Rint -> Some Eq
  | Rbool, Rbool -> Some Eq
  | Rpair (a1, a2), Rpair (b1, b2) ->
      begin match rep_equal a1 b1 with
      | None -> None
      | Some Eq -> match rep_equal a2 b2 with
        | None -> None
        | Some Eq -> Some Eq
      end
  | Rfun (a1, a2), Rfun (b1, b2) ->
      begin match rep_equal a1 b1 with
      | None -> None
      | Some Eq -> match rep_equal a2 b2 with
        | None -> None
        | Some Eq -> Some Eq
      end
  | _ -> None
;;

type assoc = Assoc : string * 'a rep * 'a -> assoc

let rec assoc : type a. string -> a rep -> assoc list -> a =
  fun x r -> function
  | [] -> raise Not_found
  | Assoc (x', r', v) :: env ->
      if x = x' then
        match rep_equal r r' with
        | None -> failwith ("Wrong type for " ^ x)
        | Some Eq -> v
      else assoc x r env

type _ term =
  | Var   : string * 'a rep -> 'a term
  | Abs   : string * 'a rep * 'b term -> ('a -> 'b) term
  | Const : int -> int term
  | Add   : (int * int -> int) term
  | LT    : (int * int -> bool) term
  | Ap    : ('a -> 'b) term * 'a term -> 'b term
  | Pair  : 'a term * 'b term -> ('a * 'b) term

let rec eval_term : type a. assoc list -> a term -> a =
  fun env -> function
  | Var (x, r) -> assoc x r env
  | Abs (x, r, e) -> fun v -> eval_term (Assoc (x, r, v) :: env) e
  | Const x -> x
  | Add -> fun (x,y) -> x+y
  | LT  -> fun (x,y) -> x<y
  | Ap(f,x) -> eval_term env f (eval_term env x)
  | Pair(x,y) -> (eval_term env x, eval_term env y)
;;

let ex3 = Abs ("x", Rint, Ap (Add, Pair (Var("x",Rint), Var("x",Rint))))
let ex4 = Ap (ex3, Const 3)

let v4 = eval_term [] ex4
;;

(* 5.9/5.10 Language with binding *)

type rnil = RNIL
type ('a,'b,'c) rcons = RCons of 'a * 'b * 'c

type _ is_row =
  | Rnil  : rnil is_row
  | Rcons : 'c is_row -> ('a,'b,'c) rcons is_row

type (_,_) lam =
  | Const : int -> ('e, int) lam
  | Var : 'a -> (('a,'t,'e) rcons, 't) lam
  | Shift : ('e,'t) lam -> (('a,'q,'e) rcons, 't) lam
  | Abs : 'a * (('a,'s,'e) rcons, 't) lam -> ('e, 's -> 't) lam
  | App : ('e, 's -> 't) lam * ('e, 's) lam -> ('e, 't) lam

type x = X
type y = Y

let ex1 = App (Var X, Shift (Var Y))
let ex2 = Abs (X, Abs (Y, App (Shift (Var X), Var Y)))
;;

type _ env =
  | Enil : rnil env
  | Econs : 'a * 't * 'e env -> ('a, 't, 'e) rcons env

let rec eval_lam : type e t. e env -> (e, t) lam -> t =
  fun env m ->
  match env, m with
  | _, Const n -> n
  | Econs (_, v, r), Var _ -> v
  | Econs (_, _, r), Shift e -> eval_lam r e
  | _, Abs (n, body) -> fun x -> eval_lam (Econs (n, x, env)) body
  | _, App (f, x)    -> eval_lam env f (eval_lam env x)
;;

type add = Add
type suc = Suc

let env0 = Econs (Zero, 0, Econs (Suc, succ, Econs (Add, (+), Enil)))

let _0 : (_, int) lam = Var Zero
let suc x = App (Shift (Var Suc : (_, int -> int) lam), x)
let _1 = suc _0
let _2 = suc _1
let _3 = suc _2
let add = Shift (Shift (Var Add : (_, int -> int -> int) lam))

let double = Abs (X, App (App (Shift add, Var X), Var X))
let ex3 = App (double, _3)
;;

let v3 = eval_lam env0 ex3
;;

(* 5.13: Constructing typing derivations at runtime *)

(* Modified slightly to use the language of 5.10, since this is more fun.
   Of course this works also with the language of 5.12. *)

type _ rep =
  | I : int rep
  | Ar : 'a rep * 'b rep -> ('a -> 'b) rep

let rec compare : type a b. a rep -> b rep -> (string, (a,b) equal) sum =
  fun a b ->
  match a, b with
  | I, I -> Inr Eq
  | Ar(x,y), Ar(s,t) ->
      begin match compare x s with
      | Inl _ as e -> e
      | Inr Eq -> match compare y t with
        | Inl _ as e -> e
        | Inr Eq as e -> e
      end
  | I, Ar _ -> Inl "I <> Ar _"
  | Ar _, I -> Inl "Ar _ <> I"
;;

type term =
  | C of int
  | Ab : string * 'a rep * term -> term
  | Ap of term * term
  | V of string

type _ ctx =
  | Cnil : rnil ctx
  | Ccons : 't * string * 'x rep * 'e ctx -> ('t,'x,'e) rcons ctx
;;

type _ checked =
  | Cerror of string
  | Cok : ('e,'t) lam * 't rep -> 'e checked

let rec lookup : type e. string -> e ctx -> e checked =
  fun name ctx ->
  match ctx with
  | Cnil -> Cerror ("Name not found: " ^ name)
  | Ccons (l,s,t,rs) ->
      if s = name then Cok (Var l,t) else
      match lookup name rs with
      | Cerror m -> Cerror m
      | Cok (v, t) -> Cok (Shift v, t)
;;

let rec tc : type n e. n nat -> e ctx -> term -> e checked =
  fun n ctx t ->
  match t with
  | V s -> lookup s ctx
  | Ap(f,x) ->
      begin match tc n ctx f with
      | Cerror _ as e -> e
      | Cok (f', ft) -> match tc n ctx x with
        | Cerror _ as e -> e
        | Cok (x', xt) ->
            match ft with
            | Ar (a, b) ->
                begin match compare a xt with
                | Inl s -> Cerror s
                | Inr Eq -> Cok (App (f',x'), b)
                end
            | _ -> Cerror "Non fun in Ap"
      end
  | Ab(s,t,body) ->
      begin match tc (NS n) (Ccons (n, s, t, ctx)) body with
      | Cerror _ as e -> e
      | Cok (body', et) -> Cok (Abs (n, body'), Ar (t, et))
      end
  | C m -> Cok (Const m, I)
;;

let ctx0 =
  Ccons (Zero, "0", I,
         Ccons (Suc, "S", Ar(I,I),
                Ccons (Add, "+", Ar(I,Ar(I,I)), Cnil)))

let ex1 = Ab ("x", I, Ap(Ap(V"+",V"x"),V"x"));;
let c1 = tc NZ ctx0 ex1;;
let ex2 = Ap (ex1, C 3);;
let c2 = tc NZ ctx0 ex2;;

let eval_checked env = function
  | Cerror s -> failwith s
  | Cok (e, I) -> (eval_lam env e : int)
  | Cok _ -> failwith "Can only evaluate expressions of type I"
;;

let v2 = eval_checked env0 c2 ;;

(* 5.12 Soundness *)

type pexp = PEXP
type pval = PVAL
type _ mode =
  | Pexp : pexp mode
  | Pval : pval mode

type ('a,'b) tarr = TARR
type tint = TINT

type (_,_) rel =
  | IntR : (tint, int) rel
  | IntTo : ('b, 's) rel -> ((tint, 'b) tarr, int -> 's) rel

type (_,_,_) lam =
  | Const : ('a,'b) rel * 'b -> (pval, 'env, 'a) lam
  | Var : 'a -> (pval, ('a,'t,'e) rcons, 't) lam
  | Shift : ('m,'e,'t) lam -> ('m, ('a,'q,'e) rcons, 't) lam
  | Lam : 'a * ('m, ('a,'s,'e) rcons, 't) lam -> (pval, 'e, ('s,'t) tarr) lam
  | App : ('m1, 'e, ('s,'t) tarr) lam * ('m2, 'e, 's) lam -> (pexp, 'e, 't) lam
;;

let ex1 = App (Lam (X, Var X), Const (IntR, 3))

let rec mode : type m e t. (m,e,t) lam -> m mode = function
  | Lam (v, body) -> Pval
  | Var v -> Pval
  | Const (r, v) -> Pval
  | Shift e -> mode e
  | App _ -> Pexp
;;

type (_,_) sub =
  | Id : ('r,'r) sub
  | Bind : 't * ('m,'r2,'x) lam * ('r,'r2) sub -> (('t,'x,'r) rcons, 'r2) sub
  | Push : ('r1,'r2) sub -> (('a,'b,'r1) rcons, ('a,'b,'r2) rcons) sub

type (_,_) lam' = Ex : ('m, 's, 't) lam -> ('s,'t) lam'
;;

let rec subst : type m1 r t s. (m1,r,t) lam -> (r,s) sub -> (s,t) lam' =
  fun t s ->
  match t, s with
  | _, Id -> Ex t
  | Const(r,c), sub -> Ex (Const (r,c))
  | Var v, Bind (x, e, r) -> Ex e
  | Var v, Push sub -> Ex (Var v)
  | Shift e, Bind (_, _, r) -> subst e r
  | Shift e, Push sub ->
      (match subst e sub with Ex a -> Ex (Shift a))
  | App(f,x), sub ->
      (match subst f sub, subst x sub with Ex g, Ex y -> Ex (App (g,y)))
  | Lam(v,x), sub ->
      (match subst x (Push sub) with Ex body -> Ex (Lam (v, body)))
;;

type closed = rnil

type 'a rlam = ((pexp,closed,'a) lam, (pval,closed,'a) lam) sum ;;

let rec rule : type a b.
  (pval, closed, (a,b) tarr) lam -> (pval, closed, a) lam -> b rlam =
  fun v1 v2 ->
  match v1, v2 with
  | Lam(x,body), v ->
      begin
        match subst body (Bind (x, v, Id)) with Ex term ->
        match mode term with
        | Pexp -> Inl term
        | Pval -> Inr term
      end
  | Const (IntTo b, f), Const (IntR, x) ->
      Inr (Const (b, f x))
;;
let rec onestep : type m t. (m,closed,t) lam -> t rlam = function
  | Lam (v, body) -> Inr (Lam (v, body))
  | Const (r, v)  -> Inr (Const (r, v))
  | App (e1, e2) ->
      match mode e1, mode e2 with
      | Pexp, _->
          begin match onestep e1 with
          | Inl e -> Inl(App(e,e2))
          | Inr v -> Inl(App(v,e2))
          end
      | Pval, Pexp ->
          begin match onestep e2 with
          | Inl e -> Inl(App(e1,e))
          | Inr v -> Inl(App(e1,v))
          end
      | Pval, Pval -> rule e1 e2
;;
