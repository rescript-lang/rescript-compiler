open StdLabels
open MoreLabels

(* Use maps for substitutions and sets for free variables *)

module Subst = Map.Make(struct type t = string let compare = compare end)
module Names = Set.Make(struct type t = string let compare = compare end)


(* Variables are common to lambda and expr *)

type var = [`Var of string]

let subst_var ~subst : var -> _ =
  function `Var s as x ->
    try Subst.find s subst
    with Not_found -> x

let free_var : var -> _ = function `Var s -> Names.singleton s


(* The lambda language: free variables, substitutions, and evaluation *)

type 'a lambda = [`Var of string | `Abs of string * 'a | `App of 'a * 'a]

let free_lambda ~free_rec : _ lambda -> _ = function
    #var as x -> free_var x
  | `Abs (s, t) -> Names.remove s (free_rec t)
  | `App (t1, t2) -> Names.union (free_rec t1) (free_rec t2)

let map_lambda ~map_rec : _ lambda -> _ = function
    #var as x -> x
  | `Abs (s, t) as l ->
      let t' = map_rec t in
      if t == t' then l else `Abs(s, t')
  | `App (t1, t2) as l ->
      let t'1 = map_rec t1 and t'2 = map_rec t2 in
      if t'1 == t1 && t'2 == t2 then l else `App (t'1, t'2)

let next_id =
  let current = ref 3 in
  fun () -> incr current; !current

let subst_lambda ~subst_rec ~free ~subst : _ lambda -> _ = function
    #var as x -> subst_var ~subst x
  | `Abs(s, t) as l ->
      let used = free t in
      let used_expr =
        Subst.fold subst ~init:[]
          ~f:(fun ~key ~data acc ->
                if Names.mem s used then data::acc else acc) in
      if List.exists used_expr ~f:(fun t -> Names.mem s (free t)) then
        let name = s ^ string_of_int (next_id ()) in
        `Abs(name,
             subst_rec ~subst:(Subst.add ~key:s ~data:(`Var name) subst) t)
      else
        map_lambda ~map_rec:(subst_rec ~subst:(Subst.remove s subst)) l
  | `App _ as l ->
      map_lambda ~map_rec:(subst_rec ~subst) l

let eval_lambda ~eval_rec ~subst l =
  match map_lambda ~map_rec:eval_rec l with
    `App(`Abs(s,t1), t2) ->
      eval_rec (subst ~subst:(Subst.add ~key:s ~data:t2 Subst.empty) t1)
  | t -> t

(* Specialized versions to use on lambda *)

let rec free1 x = free_lambda ~free_rec:free1 x
let rec subst1 ~subst = subst_lambda ~subst_rec:subst1 ~free:free1 ~subst
let rec eval1 x = eval_lambda ~eval_rec:eval1 ~subst:subst1 x


(* The expr language of arithmetic expressions *)

type 'a expr =
    [`Var of string | `Num of int | `Add of 'a * 'a
    | `Neg of 'a | `Mult of 'a * 'a]

let free_expr ~free_rec : _ expr -> _ = function
    #var as x -> free_var x
  | `Num _ -> Names.empty
  | `Add(x, y) -> Names.union (free_rec x) (free_rec y)
  | `Neg x -> free_rec x
  | `Mult(x, y) -> Names.union (free_rec x) (free_rec y)

(* Here map_expr helps a lot *)
let map_expr ~map_rec : _ expr -> _ = function
    #var as x -> x
  | `Num _ as x -> x
  | `Add(x, y) as e ->
      let x' = map_rec x and y' = map_rec y in
      if x == x' && y == y' then e
      else `Add(x', y')
  | `Neg x as e ->
      let x' = map_rec x in
      if x == x' then e else `Neg x'
  | `Mult(x, y) as e ->
      let x' = map_rec x and y' = map_rec y in
      if x == x' && y == y' then e
      else `Mult(x', y')

let subst_expr ~subst_rec ~subst : _ expr -> _ = function
    #var as x -> subst_var ~subst x
  | #expr as e -> map_expr ~map_rec:(subst_rec ~subst) e

let eval_expr ~eval_rec e =
  match map_expr ~map_rec:eval_rec e with
    `Add(`Num m, `Num n) -> `Num (m+n)
  | `Neg(`Num n) -> `Num (-n)
  | `Mult(`Num m, `Num n) -> `Num (m*n)
  | #expr as e -> e

(* Specialized versions *)

let rec free2 x = free_expr ~free_rec:free2 x
let rec subst2 ~subst = subst_expr ~subst_rec:subst2 ~subst
let rec eval2 x = eval_expr ~eval_rec:eval2 x


(* The lexpr language, reunion of lambda and expr *)

type lexpr =
  [ `Var of string | `Abs of string * lexpr | `App of lexpr * lexpr
  | `Num of int | `Add of lexpr * lexpr | `Neg of lexpr
  | `Mult of lexpr * lexpr ]

let rec free : lexpr -> _ = function
    #lambda as x -> free_lambda ~free_rec:free x
  | #expr as x -> free_expr ~free_rec:free x

let rec subst ~subst:s : lexpr -> _ = function
    #lambda as x -> subst_lambda ~subst_rec:subst ~subst:s ~free x
  | #expr as x -> subst_expr ~subst_rec:subst ~subst:s x

let rec eval : lexpr -> _ = function
    #lambda as x -> eval_lambda ~eval_rec:eval ~subst x
  | #expr as x -> eval_expr ~eval_rec:eval x

let rec print = function
  | `Var id -> print_string id
  | `Abs (id, l) -> print_string ("\ " ^ id ^ " . "); print l
  | `App (l1, l2) -> print l1; print_string " "; print l2
  | `Num x -> print_int x
  | `Add (e1, e2) -> print e1; print_string " + "; print e2
  | `Neg e -> print_string "-"; print e
  | `Mult (e1, e2) -> print e1; print_string " * "; print e2

let () =
  let e1 = eval1 (`App(`Abs("x",`Var"x"), `Var"y")) in
  let e2 = eval2 (`Add(`Mult(`Num 3,`Neg(`Num 2)), `Var"x")) in
  let e3 = eval (`Add(`App(`Abs("x",`Mult(`Var"x",`Var"x")),`Num 2), `Num 5)) in
  print e1; print_newline ();
  print e2; print_newline ();
  print e3; print_newline ()
