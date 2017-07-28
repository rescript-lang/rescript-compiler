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

(****************** Term manipulations *****************)

type term =
    Var of int
  | Term of string * term list

let rec union l1 l2 =
  match l1 with
    []   -> l2
  | a::r -> if List.mem a l2 then union r l2 else a :: union r l2


let rec vars = function
    Var n -> [n]
  | Term(_,l) -> vars_of_list l
and vars_of_list = function
    [] -> []
  | t::r -> union (vars t) (vars_of_list r)


let rec substitute subst = function
    Term(oper,sons) -> Term(oper, List.map (substitute subst) sons)
  | Var(n) as t     -> try List.assoc n subst with Not_found -> t


(* Term replacement: replace M u N is M[u<-N]. *)

let rec replace m u n =
  match (u, m) with
    [], _ -> n
  | i::u, Term(oper, sons) -> Term(oper, replace_nth i sons u n)
  | _ -> failwith "replace"

and replace_nth i sons u n =
  match sons with
    s::r -> if i = 1
            then replace s u n :: r
            else s :: replace_nth (i-1) r u n
  | []   -> failwith "replace_nth"


(* Term matching. *)

let matching term1 term2 =
  let rec match_rec subst t1 t2 =
    match (t1, t2) with
      Var v, _ ->
        if List.mem_assoc v subst then
          if t2 = List.assoc v subst then subst else failwith "matching"
        else
          (v, t2) :: subst
    | Term(op1,sons1), Term(op2,sons2) ->
        if op1 = op2
        then List.fold_left2 match_rec subst sons1 sons2
        else failwith "matching"
    | _ -> failwith "matching" in
  match_rec [] term1 term2


(* A naive unification algorithm. *)

let compsubst subst1 subst2 =
  (List.map (fun (v,t) -> (v, substitute subst1 t)) subst2) @ subst1


let rec occurs n = function
    Var m -> m = n
  | Term(_,sons) -> List.exists (occurs n) sons


let rec unify term1 term2 =
  match (term1, term2) with
    Var n1, _ ->
      if term1 = term2 then []
      else if occurs n1 term2 then failwith "unify"
      else [n1, term2]
  | term1, Var n2 ->
      if occurs n2 term1 then failwith "unify"
      else [n2, term1]
  | Term(op1,sons1), Term(op2,sons2) ->
      if op1 = op2 then
        List.fold_left2 (fun s t1 t2 -> compsubst (unify (substitute s t1)
                                                         (substitute s t2)) s)
                   [] sons1 sons2
      else failwith "unify"


(* We need to print terms with variables independently from input terms
  obtained by parsing. We give arbitrary names v1,v2,... to their variables.
*)

let infixes = ["+";"*"]

let rec pretty_term = function
    Var n ->
      print_string "v"; print_int n
  | Term (oper,sons) ->
      if List.mem oper infixes then begin
        match sons with
            [s1;s2] ->
              pretty_close s1; print_string oper; pretty_close s2
          | _ ->
              failwith "pretty_term : infix arity <> 2"
      end else begin
        print_string oper;
        match sons with
             []   -> ()
          | t::lt -> print_string "(";
                     pretty_term t;
                     List.iter (fun t -> print_string ","; pretty_term t) lt;
                     print_string ")"
      end

and pretty_close = function
    Term(oper, _) as m ->
      if List.mem oper infixes then begin
        print_string "("; pretty_term m; print_string ")"
      end else
        pretty_term m
  | m ->
      pretty_term m
