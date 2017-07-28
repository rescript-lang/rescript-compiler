(* Injectivity *)

type (_, _) eq = Refl : ('a, 'a) eq

let magic : 'a 'b. 'a -> 'b =
  fun (type a) (type b) (x : a) ->
    let module M =
      (functor (T : sig type 'a t end) ->
       struct
         let f (Refl : (a T.t, b T.t) eq) = (x :> b)
       end)
        (struct type 'a t = unit end)
    in M.f Refl
;;

(* Variance and subtyping *)

type (_, +_) eq = Refl : ('a, 'a) eq

let magic : 'a 'b. 'a -> 'b =
  fun (type a) (type b) (x : a) ->
    let bad_proof (type a) =
      (Refl : (< m : a>, <m : a>) eq :> (<m : a>, < >) eq) in
    let downcast : type a. (a, < >) eq -> < > -> a =
      fun (type a) (Refl : (a, < >) eq) (s : < >) -> (s :> a) in
    (downcast bad_proof ((object method m = x end) :> < >)) # m
;;

(* Record patterns *)

type _ t =
  | IntLit : int t
  | BoolLit : bool t

let check : type s . s t * s -> bool = function
  | BoolLit, false -> false
  | IntLit , 6 -> false
;;

type ('a, 'b) pair = { fst : 'a; snd : 'b }

let check : type s . (s t, s) pair -> bool = function
  | {fst = BoolLit; snd = false} -> false
  | {fst = IntLit ; snd =  6} -> false
;;
