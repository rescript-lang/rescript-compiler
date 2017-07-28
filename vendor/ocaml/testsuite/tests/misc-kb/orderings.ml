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

(*********************** Recursive Path Ordering ****************************)

open Terms

type ordering =
    Greater
  | Equal
  | NotGE

let ge_ord order pair = match order pair with NotGE -> false | _ -> true
and gt_ord order pair = match order pair with Greater -> true | _ -> false
and eq_ord order pair = match order pair with Equal -> true | _ -> false


let rec rem_eq equiv x = function
     []  -> failwith "rem_eq"
  | y::l -> if equiv (x,y) then l else y :: rem_eq equiv x l


let diff_eq equiv (x,y) =
  let rec diffrec = function
      ([],_) as p -> p
    | (h::t, y)   -> try
                       diffrec (t, rem_eq equiv h y)
                     with Failure _ ->
                       let (x',y') = diffrec (t,y) in (h::x',y') in
  if List.length x > List.length y then diffrec(y,x) else diffrec(x,y)


(* Multiset extension of order *)

let mult_ext order = function
    Term(_,sons1), Term(_,sons2) ->
      begin match diff_eq (eq_ord order) (sons1,sons2) with
        ([],[]) -> Equal
      | (l1,l2) ->
           if List.for_all
                (fun n -> List.exists (fun m -> gt_ord order (m,n)) l1) l2
           then Greater else NotGE
      end
  | _ -> failwith "mult_ext"


(* Lexicographic extension of order *)

let lex_ext order = function
    (Term(_,sons1) as m), (Term(_,sons2) as n) ->
      let rec lexrec = function
        ([] , []) -> Equal
      | ([] , _ ) -> NotGE
      | ( _ , []) -> Greater
      | (x1::l1, x2::l2) ->
          match order (x1,x2) with
            Greater -> if List.for_all (fun n' -> gt_ord order (m,n')) l2
                       then Greater else NotGE
          | Equal -> lexrec (l1,l2)
          | NotGE -> if List.exists (fun m' -> ge_ord order (m',n)) l1
                     then Greater else NotGE in
      lexrec (sons1, sons2)
  | _ -> failwith "lex_ext"


(* Recursive path ordering *)

let rpo op_order ext =
  let rec rporec (m,n) =
    if m = n then Equal else
      match m with
          Var vm -> NotGE
        | Term(op1,sons1) ->
            match n with
                Var vn ->
                  if occurs vn m then Greater else NotGE
              | Term(op2,sons2) ->
                  match (op_order op1 op2) with
                      Greater ->
                        if List.for_all (fun n' -> gt_ord rporec (m,n')) sons2
                        then Greater else NotGE
                    | Equal ->
                        ext rporec (m,n)
                    | NotGE ->
                        if List.exists (fun m' -> ge_ord rporec (m',n)) sons1
                        then Greater else NotGE
  in rporec
