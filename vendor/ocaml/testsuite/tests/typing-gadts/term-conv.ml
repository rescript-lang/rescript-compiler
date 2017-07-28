(* HOAS to de Bruijn, by chak *)
(* http://www.cse.unsw.edu.au/~chak/haskell/term-conv/ *)

module Typeable = struct
  type 'a ty =
    | Int: int ty
    | String: string ty
    | List: 'a ty -> 'a list ty
    | Pair: ('a ty * 'b ty) -> ('a * 'b) ty
    | Fun: ('a ty * 'b ty) -> ('a -> 'b) ty

  type (_,_) eq = Eq : ('a,'a) eq

  exception CastFailure
  let rec check_eq : type t t'. t ty -> t' ty -> (t,t') eq = fun t t' ->
    match t, t' with
    | Int, Int -> Eq
    | String, String -> Eq
    | List t, List t' -> (match check_eq t t' with Eq -> Eq)
    | Pair (t1,t2), Pair (t1',t2') ->
        (match check_eq t1 t1', check_eq t2 t2' with Eq, Eq -> Eq)
    | Fun (t1,t2), Fun (t1',t2') ->
        (match check_eq t1 t1', check_eq t2 t2' with Eq, Eq -> Eq)
    | _ -> raise CastFailure

  let gcast : type t t'. t ty -> t' ty -> t -> t' = fun t t' x ->
    match check_eq t t' with Eq -> x
end;;

module HOAS = struct
  open Typeable

  type _ term =
    | Tag : 't ty * int -> 't term
    | Con : 't -> 't term
    | Lam : 's ty * ('s term -> 't term) -> ('s -> 't) term
    | App : ('s -> 't) term * 's term -> 't term

  let rec intp : type t. t term -> t = function
    | Tag (_, ix) -> failwith "HOAS.intp"
    | Con v      -> v
    | Lam (_, f) -> fun x -> intp (f (Con x))
    | App (f, a) -> intp f (intp a)
end;;

module DeBruijn = struct
  type ('env,'t) ix =
    | ZeroIx : ('env * 't, 't) ix
    | SuccIx : ('env,'t) ix -> ('env * 's, 't) ix

  let rec to_int : type env t. (env,t) ix -> int = function
    | ZeroIx   -> 0
    | SuccIx n -> to_int n + 1

  type ('env,'t) term =
    | Var : ('env,'t) ix -> ('env,'t) term
    | Con : 't -> ('env,'t) term
    | Lam : ('env * 's, 't) term -> ('env, 's -> 't) term
    | App : ('env, 's -> 't) term * ('env, 's) term -> ('env, 't) term

  type _ stack =
    | Empty : unit stack
    | Push : 'env stack * 't -> ('env * 't) stack

  let rec prj : type env t. (env,t) ix -> env stack -> t = fun i s ->
    match i, s with
    | ZeroIx, Push (s,v) -> v
    | SuccIx i, Push (s,_) -> prj i s

  let rec intp : type env t. (env,t) term -> env stack -> t = fun t s ->
    match t with
    | Var ix -> prj ix s
    | Con v  -> v
    | Lam b  -> fun x -> intp b (Push (s, x))
    | App(f,a) -> intp f s (intp a s)
end;;

module Convert = struct
  type (_,_) layout =
    | EmptyLayout : ('env, unit) layout
    | PushLayout  :
        't Typeable.ty * ('env,'env') layout * ('env,'t) DeBruijn.ix
           -> ('env,'env' * 't) layout

  let rec size : type env env'. (env,env') layout -> int = function
    | EmptyLayout -> 0
    | PushLayout (_, lyt, _) -> size lyt + 1

  let rec inc : type env env'. (env,env') layout -> (env * 't, env') layout =
    function
      | EmptyLayout -> EmptyLayout
      | PushLayout (t, lyt, ix) -> PushLayout (t, inc lyt, DeBruijn.SuccIx ix)

  let rec prj : type env env' t.
        t Typeable.ty -> int -> (env,env') layout -> (env,t) DeBruijn.ix
    = fun t n -> function
      | EmptyLayout -> failwith "Convert.prj: internal error"
      | PushLayout (t', l, ix) ->
          if n = 0 then
            match Typeable.check_eq t t' with Typeable.Eq -> ix
          else prj t (n-1) l

  let rec cvt :
    type env t. (env,env) layout -> t HOAS.term -> (env,t) DeBruijn.term =
    fun lyt -> function
      | HOAS.Tag (t, sz) -> DeBruijn.Var (prj t (size lyt - sz -1) lyt)
      | HOAS.Con v -> DeBruijn.Con v
      | HOAS.Lam (t, f) ->
          let lyt' = PushLayout (t, inc lyt, DeBruijn.ZeroIx) in
          DeBruijn.Lam (cvt lyt' (f (HOAS.Tag (t, size lyt))))
      | HOAS.App (f, a) ->
          DeBruijn.App (cvt lyt f, cvt lyt a)

  let convert t = cvt EmptyLayout t
end;;

module Main = struct
  open HOAS
  let i t = Lam (t, fun x -> x)
  let zero t = Lam (Typeable.Fun(t,t), fun f -> Lam(t, fun x -> x))
  let one t = Lam (Typeable.Fun(t,t), fun f -> Lam(t, fun x -> App (f, x)))
  let two t =
    Lam (Typeable.Fun(t,t), fun f -> Lam(t, fun x -> App (f, App (f, x))))
  let three t =
    Lam (Typeable.Fun(t,t),
         fun f -> Lam(t, fun x -> App (f, App (f, App (f, x)))))
  let plus t =
    let t1 = Typeable.Fun(t,t) in let t2 = Typeable.Fun(t1,t1) in
    Lam (t2, fun m -> Lam (t2, fun n ->
      Lam (t1, fun f -> Lam(t, fun x -> App(App(m,f), App(App(n,f),x))))))

  let plus_2_3 t = App (App (plus t, two t), three t)

  open Convert

  let i' = convert (i Typeable.Int)
  let plus_2_3' = convert (plus_2_3 Typeable.Int)
  let eval_plus_2_3' = DeBruijn.intp plus_2_3' DeBruijn.Empty succ 0
end;;
