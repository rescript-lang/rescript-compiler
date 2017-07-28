(* Full fledge version, using objects to structure code *)

open StdLabels
open MoreLabels

(* Use maps for substitutions and sets for free variables *)

module Subst = Map.Make(struct type t = string let compare = compare end)
module Names = Set.Make(struct type t = string let compare = compare end)

(* To build recursive objects *)

let lazy_fix make =
  let rec obj () = make (lazy (obj ()) : _ Lazy.t) in
  obj ()

let (!!) = Lazy.force

(* The basic operations *)

class type ['a, 'b] ops =
  object
    method free : 'b -> Names.t
    method subst : sub:'a Subst.t -> 'b -> 'a
    method eval : 'b -> 'a
  end

(* Variables are common to lambda and expr *)

type var = [`Var of string]

class ['a] var_ops = object (self : ('a, var) #ops)
  constraint 'a = [> var]
  method subst ~sub (`Var s as x) =
    try Subst.find s sub with Not_found -> x
  method free (`Var s) =
    Names.singleton s
  method eval (#var as v) = v
end

(* The lambda language: free variables, substitutions, and evaluation *)

type 'a lambda = [`Var of string | `Abs of string * 'a | `App of 'a * 'a]

let next_id =
  let current = ref 3 in
  fun () -> incr current; !current

class ['a] lambda_ops (ops : ('a,'a) #ops Lazy.t) =
  let var : 'a var_ops = new var_ops
  and free = lazy !!ops#free
  and subst = lazy !!ops#subst
  and eval = lazy !!ops#eval in
  object (self : ('a, 'a lambda) #ops)
    constraint 'a = [> 'a lambda]
    method free = function
        #var as x -> var#free x
      | `Abs (s, t) -> Names.remove s (!!free t)
      | `App (t1, t2) -> Names.union (!!free t1) (!!free t2)

    method map ~f = function
        #var as x -> x
      | `Abs (s, t) as l ->
          let t' = f t in
          if t == t' then l else `Abs(s, t')
      | `App (t1, t2) as l ->
          let t'1 = f t1 and t'2 = f t2 in
          if t'1 == t1 && t'2 == t2 then l else `App (t'1, t'2)

    method subst ~sub = function
        #var as x -> var#subst ~sub x
      | `Abs(s, t) as l ->
          let used = !!free t in
          let used_expr =
            Subst.fold sub ~init:[]
              ~f:(fun ~key ~data acc ->
                if Names.mem s used then data::acc else acc) in
          if List.exists used_expr ~f:(fun t -> Names.mem s (!!free t)) then
            let name = s ^ string_of_int (next_id ()) in
            `Abs(name,
                 !!subst ~sub:(Subst.add ~key:s ~data:(`Var name) sub) t)
          else
            self#map ~f:(!!subst ~sub:(Subst.remove s sub)) l
      | `App _ as l ->
          self#map ~f:(!!subst ~sub) l

    method eval l =
      match self#map ~f:!!eval l with
        `App(`Abs(s,t1), t2) ->
          !!eval (!!subst ~sub:(Subst.add ~key:s ~data:t2 Subst.empty) t1)
      | t -> t
end

(* Operations specialized to lambda *)

let lambda = lazy_fix (new lambda_ops)

(* The expr language of arithmetic expressions *)

type 'a expr =
    [ `Var of string | `Num of int | `Add of 'a * 'a
    | `Neg of 'a | `Mult of 'a * 'a]

class ['a] expr_ops (ops : ('a,'a) #ops Lazy.t) =
  let var : 'a var_ops = new var_ops
  and free = lazy !!ops#free
  and subst = lazy !!ops#subst
  and eval = lazy !!ops#eval in
  object (self : ('a, 'a expr) #ops)
    constraint 'a = [> 'a expr]
    method free = function
        #var as x -> var#free x
      | `Num _ -> Names.empty
      | `Add(x, y) -> Names.union (!!free x) (!!free y)
      | `Neg x -> !!free x
      | `Mult(x, y) -> Names.union (!!free x) (!!free y)

    method map ~f = function
        #var as x -> x
      | `Num _ as x -> x
      | `Add(x, y) as e ->
          let x' = f x and y' = f y in
          if x == x' && y == y' then e
          else `Add(x', y')
      | `Neg x as e ->
          let x' = f x in
          if x == x' then e else `Neg x'
      | `Mult(x, y) as e ->
          let x' = f x and y' = f y in
          if x == x' && y == y' then e
          else `Mult(x', y')

    method subst ~sub = function
        #var as x -> var#subst ~sub x
      | #expr as e -> self#map ~f:(!!subst ~sub) e

    method eval (#expr as e) =
      match self#map ~f:!!eval e with
        `Add(`Num m, `Num n) -> `Num (m+n)
      | `Neg(`Num n) -> `Num (-n)
      | `Mult(`Num m, `Num n) -> `Num (m*n)
      | e -> e
  end

(* Specialized versions *)

let expr = lazy_fix (new expr_ops)

(* The lexpr language, reunion of lambda and expr *)

type 'a lexpr = [ 'a lambda | 'a expr ]

class ['a] lexpr_ops (ops : ('a,'a) #ops Lazy.t) =
  let lambda = new lambda_ops ops in
  let expr = new expr_ops ops in
  object (self : ('a, 'a lexpr) #ops)
    constraint 'a = [> 'a lexpr]
    method free = function
        #lambda as x -> lambda#free x
      | #expr as x -> expr#free x

    method subst ~sub = function
        #lambda as x -> lambda#subst ~sub x
      | #expr as x -> expr#subst ~sub x

    method eval = function
        #lambda as x -> lambda#eval x
      | #expr as x -> expr#eval x
end

let lexpr = lazy_fix (new lexpr_ops)

let rec print = function
  | `Var id -> print_string id
  | `Abs (id, l) -> print_string ("\ " ^ id ^ " . "); print l
  | `App (l1, l2) -> print l1; print_string " "; print l2
  | `Num x -> print_int x
  | `Add (e1, e2) -> print e1; print_string " + "; print e2
  | `Neg e -> print_string "-"; print e
  | `Mult (e1, e2) -> print e1; print_string " * "; print e2

let () =
  let e1 = lambda#eval (`App(`Abs("x",`Var"x"), `Var"y")) in
  let e2 = expr#eval (`Add(`Mult(`Num 3,`Neg(`Num 2)), `Var"x")) in
  let e3 =
    lexpr#eval (`Add(`App(`Abs("x",`Mult(`Var"x",`Var"x")),`Num 2), `Num 5))
  in
  print e1; print_newline ();
  print e2; print_newline ();
  print e3; print_newline ()
