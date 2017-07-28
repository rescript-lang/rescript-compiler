(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(****************** Equation manipulations *************)

open Terms

type rule =
  { number: int;
    numvars: int;
    lhs: term;
    rhs: term }

(* standardizes an equation so its variables are 1,2,... *)

let mk_rule num m n =
  let all_vars = union (vars m) (vars n) in
  let counter = ref 0 in
  let subst =
    List.map (fun v -> incr counter; (v, Var !counter)) (List.rev all_vars) in
  { number = num;
    numvars = !counter;
    lhs = substitute subst m;
    rhs = substitute subst n }


(* checks that rules are numbered in sequence and returns their number *)

let check_rules rules =
  let counter = ref 0 in
  List.iter (fun r -> incr counter;
                 if r.number <> !counter
                 then failwith "Rule numbers not in sequence")
       rules;
  !counter


let pretty_rule rule =
  print_int rule.number; print_string " : ";
  pretty_term rule.lhs; print_string " = "; pretty_term rule.rhs;
  print_newline()


let pretty_rules rules = List.iter pretty_rule rules

(****************** Rewriting **************************)

(* Top-level rewriting. Let eq:L=R be an equation, M be a term such that L<=M.
   With sigma = matching L M, we define the image of M by eq as sigma(R) *)
let reduce l m r =
  substitute (matching l m) r

(* Test whether m can be reduced by l, i.e. m contains an instance of l. *)

let can_match l m =
  try let _ = matching l m in true
  with Failure _ -> false

let rec reducible l m =
  can_match l m ||
   (match m with
    | Term(_,sons) -> List.exists (reducible l) sons
    |         _     -> false)

(* Top-level rewriting with multiple rules. *)

let rec mreduce rules m =
  match rules with
    [] -> failwith "mreduce"
  | rule::rest ->
      try
        reduce rule.lhs m rule.rhs
      with Failure _ ->
        mreduce rest m


(* One step of rewriting in leftmost-outermost strategy,
   with multiple rules. Fails if no redex is found *)

let rec mrewrite1 rules m =
  try
    mreduce rules m
  with Failure _ ->
    match m with
      Var n -> failwith "mrewrite1"
    | Term(f, sons) -> Term(f, mrewrite1_sons rules sons)

and mrewrite1_sons rules = function
    [] -> failwith "mrewrite1"
  | son::rest ->
      try
        mrewrite1 rules son :: rest
      with Failure _ ->
        son :: mrewrite1_sons rules rest


(* Iterating rewrite1. Returns a normal form. May loop forever *)

let rec mrewrite_all rules m =
  try
    mrewrite_all rules (mrewrite1 rules m)
  with Failure _ ->
    m
