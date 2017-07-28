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

open Terms
open Equations

(****************** Critical pairs *********************)

(* All (u,subst) such that N/u (&var) unifies with M,
   with principal unifier subst *)

let rec super m = function
    Term(_,sons) as n ->
      let rec collate n = function
        [] -> []
      | son::rest ->
          List.map (fun (u, subst) -> (n::u, subst)) (super m son)
          @ collate (n+1) rest in
      let insides = collate 1 sons in
        begin try
          ([], unify m n) :: insides
        with Failure _ ->
          insides
        end
  | _ -> []


(* Ex :
let (m,_) = <<F(A,B)>>
and (n,_) = <<H(F(A,x),F(x,y))>> in super m n
==> [[1],[2,Term ("B",[])];                      x <- B
     [2],[2,Term ("A",[]); 1,Term ("B",[])]]     x <- A  y <- B
*)

(* All (u,subst), u&[], such that n/u unifies with m *)

let super_strict m = function
    Term(_,sons) ->
      let rec collate n = function
        [] -> []
      | son::rest ->
          List.map (fun (u, subst) -> (n::u, subst)) (super m son)
          @ collate (n+1) rest in
      collate 1 sons
    | _ -> []


(* Critical pairs of l1=r1 with l2=r2 *)
(* critical_pairs : term_pair -> term_pair -> term_pair list *)
let critical_pairs (l1,r1) (l2,r2) =
  let mk_pair (u,subst) =
     substitute subst (replace l2 u r1), substitute subst r2 in
  List.map mk_pair (super l1 l2)

(* Strict critical pairs of l1=r1 with l2=r2 *)
(* strict_critical_pairs : term_pair -> term_pair -> term_pair list *)
let strict_critical_pairs (l1,r1) (l2,r2) =
  let mk_pair (u,subst) =
    substitute subst (replace l2 u r1), substitute subst r2 in
  List.map mk_pair (super_strict l1 l2)


(* All critical pairs of eq1 with eq2 *)
let mutual_critical_pairs eq1 eq2 =
  (strict_critical_pairs eq1 eq2) @ (critical_pairs eq2 eq1)

(* Renaming of variables *)

let rename n (t1,t2) =
  let rec ren_rec = function
    Var k -> Var(k+n)
  | Term(op,sons) -> Term(op, List.map ren_rec sons) in
  (ren_rec t1, ren_rec t2)


(************************ Completion ******************************)

let deletion_message rule =
  print_string "Rule ";print_int rule.number; print_string " deleted";
  print_newline()


(* Generate failure message *)
let non_orientable (m,n) =
    pretty_term m; print_string " = "; pretty_term n; print_newline()


let rec partition p = function
    [] -> ([], [])
  | x::l -> let (l1, l2) = partition p l in
            if p x then (x::l1, l2) else (l1, x::l2)


let rec get_rule n = function
    [] -> raise Not_found
  | r::l -> if n = r.number then r else get_rule n l


(* Improved Knuth-Bendix completion procedure *)

let kb_completion greater =
  let rec kbrec j rules =
  let rec process failures (k,l) eqs =
(****
     print_string "***kb_completion "; print_int j; print_newline();
     pretty_rules rules;
     List.iter non_orientable failures;
     print_int k; print_string " "; print_int l; print_newline();
     List.iter non_orientable eqs;
***)
    match eqs with
      [] ->
        if k<l then next_criticals failures (k+1,l) else
        if l<j then next_criticals failures (1,l+1) else
        begin match failures with
            [] -> rules (* successful completion *)
          | _  -> print_string "Non-orientable equations :"; print_newline();
                  List.iter non_orientable failures;
                  failwith "kb_completion"
        end
    | (m,n)::eqs ->
        let m' = mrewrite_all rules m
        and n' = mrewrite_all rules n
        and enter_rule(left,right) =
          let new_rule = mk_rule (j+1) left right in
          pretty_rule new_rule;
          let left_reducible rule = reducible left rule.lhs in
          let (redl,irredl) = partition left_reducible rules in
          List.iter deletion_message redl;
          let right_reduce rule =
            mk_rule rule.number rule.lhs
                    (mrewrite_all (new_rule::rules) rule.rhs) in
          let irreds = List.map right_reduce irredl in
          let eqs' = List.map (fun rule -> (rule.lhs, rule.rhs)) redl in
          kbrec (j+1) (new_rule::irreds) [] (k,l) (eqs @ eqs' @ failures) in
(***
        print_string "--- Considering "; non_orientable (m', n');
***)
        if m' = n' then process failures (k,l) eqs else
        if greater(m',n') then enter_rule(m',n') else
        if greater(n',m') then enter_rule(n',m') else
        process ((m',n')::failures) (k,l) eqs

  and next_criticals failures (k,l) =
(****
    print_string "***next_criticals ";
    print_int k; print_string " "; print_int l ; print_newline();
****)
    try
      let rl = get_rule l rules in
      let el = (rl.lhs, rl.rhs) in
        if k=l then
          process failures (k,l)
                  (strict_critical_pairs el (rename rl.numvars el))
        else
          try
            let rk = get_rule k rules in
            let ek = (rk.lhs, rk.rhs) in
              process failures (k,l)
                      (mutual_critical_pairs el (rename rl.numvars ek))
          with Not_found -> next_criticals failures (k+1,l)
    with Not_found -> next_criticals failures (1,l+1)
  in process
  in kbrec


(* complete_rules is assumed locally confluent, and checked Noetherian with
  ordering greater, rules is any list of rules *)

let kb_complete greater complete_rules rules =
    let n = check_rules complete_rules
    and eqs = List.map (fun rule -> (rule.lhs, rule.rhs)) rules in
    let completed_rules =
      kb_completion greater n complete_rules [] (n,n) eqs in
    print_string "Canonical set found :"; print_newline();
    pretty_rules (List.rev completed_rules)
