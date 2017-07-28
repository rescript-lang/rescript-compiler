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
open Orderings
open Kb

(****
let group_rules = [
  { number = 1; numvars = 1;
    lhs = Term("*", [Term("U",[]); Var 1]); rhs = Var 1 };
  { number = 2; numvars = 1;
    lhs = Term("*", [Term("I",[Var 1]); Var 1]); rhs = Term("U",[]) };
  { number = 3; numvars = 3;
    lhs = Term("*", [Term("*", [Var 1; Var 2]); Var 3]);
    rhs = Term("*", [Var 1; Term("*", [Var 2; Var 3])]) }
]
****)

let geom_rules = [
  { number = 1; numvars = 1;
    lhs = Term ("*",[(Term ("U",[])); (Var 1)]);
    rhs = Var 1 };
  { number = 2; numvars = 1;
    lhs = Term ("*",[(Term ("I",[(Var 1)])); (Var 1)]);
    rhs = Term ("U",[]) };
  { number = 3; numvars = 3;
    lhs = Term ("*",[(Term ("*",[(Var 1); (Var 2)])); (Var 3)]);
    rhs = Term ("*",[(Var 1); (Term ("*",[(Var 2); (Var 3)]))]) };
  { number = 4; numvars = 0;
    lhs = Term ("*",[(Term ("A",[])); (Term ("B",[]))]);
    rhs = Term ("*",[(Term ("B",[])); (Term ("A",[]))]) };
  { number = 5; numvars = 0;
    lhs = Term ("*",[(Term ("C",[])); (Term ("C",[]))]);
    rhs = Term ("U",[]) };
  { number = 6; numvars = 0;
    lhs = Term("*",
            [(Term ("C",[]));
             (Term ("*",[(Term ("A",[])); (Term ("I",[(Term ("C",[]))]))]))]);
    rhs = Term ("I",[(Term ("A",[]))]) };
  { number = 7; numvars = 0;
    lhs = Term("*",
            [(Term ("C",[]));
             (Term ("*",[(Term ("B",[])); (Term ("I",[(Term ("C",[]))]))]))]);
    rhs = Term ("B",[]) }
]

let group_rank = function
    "U" -> 0
  | "*" -> 1
  | "I" -> 2
  | "B" -> 3
  | "C" -> 4
  | "A" -> 5
  | _ -> assert false

let group_precedence op1 op2 =
  let r1 = group_rank op1
  and r2 = group_rank op2 in
    if r1 = r2 then Equal else
    if r1 > r2 then Greater else NotGE

let group_order = rpo group_precedence lex_ext

let greater pair =
  match group_order pair with Greater -> true | _ -> false

let _ =
  for i = 1 to 20 do kb_complete greater [] geom_rules done
