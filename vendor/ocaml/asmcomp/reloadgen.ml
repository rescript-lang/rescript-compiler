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

(* Insert load/stores for pseudoregs that got assigned to stack locations. *)

open Misc
open Reg
open Mach

let access_stack r =
  try
    for i = 0 to Array.length r - 1 do
      match r.(i).loc with Stack _ -> raise Exit | _ -> ()
    done;
    false
  with Exit ->
    true

let insert_move src dst next =
  if src.loc = dst.loc
  then next
  else instr_cons (Iop Imove) [|src|] [|dst|] next

let insert_moves src dst next =
  let rec insmoves i =
    if i >= Array.length src
    then next
    else insert_move src.(i) dst.(i) (insmoves (i+1))
  in insmoves 0

class reload_generic = object (self)

val mutable redo_regalloc = false

method makereg r =
  match r.loc with
    Unknown -> fatal_error "Reload.makereg"
  | Reg _ -> r
  | Stack _ ->
      redo_regalloc <- true;
      let newr = Reg.clone r in
      (* Strongly discourage spilling this register *)
      newr.spill_cost <- 100000;
      newr

method private makeregs rv =
  let n = Array.length rv in
  let newv = Array.make n Reg.dummy in
  for i = 0 to n-1 do newv.(i) <- self#makereg rv.(i) done;
  newv

method private makereg1 rv =
  let newv = Array.copy rv in
  newv.(0) <- self#makereg rv.(0);
  newv

method reload_operation op arg res =
  (* By default, assume that arguments and results must reside
     in hardware registers. For moves, allow one arg or one
     res to be stack-allocated, but do something for
     stack-to-stack moves *)
  match op with
    Imove | Ireload | Ispill ->
      begin match arg.(0), res.(0) with
        {loc = Stack s1}, {loc = Stack s2} when s1 <> s2 ->
          ([| self#makereg arg.(0) |], res)
      | _ ->
          (arg, res)
      end
  | _ ->
      (self#makeregs arg, self#makeregs res)

method reload_test tst args =
  self#makeregs args

method private reload i =
  match i.desc with
    (* For function calls, returns, etc: the arguments and results are
       already at the correct position (e.g. on stack for some arguments).
       However, something needs to be done for the function pointer in
       indirect calls. *)
    Iend | Ireturn | Iop(Itailcall_imm _) | Iraise _ -> i
  | Iop(Itailcall_ind) ->
      let newarg = self#makereg1 i.arg in
      insert_moves i.arg newarg
        {i with arg = newarg}
  | Iop(Icall_imm _ | Iextcall _) ->
      {i with next = self#reload i.next}
  | Iop(Icall_ind) ->
      let newarg = self#makereg1 i.arg in
      insert_moves i.arg newarg
        {i with arg = newarg; next = self#reload i.next}
  | Iop op ->
      let (newarg, newres) = self#reload_operation op i.arg i.res in
      insert_moves i.arg newarg
        {i with arg = newarg; res = newres; next =
          (insert_moves newres i.res
            (self#reload i.next))}
  | Iifthenelse(tst, ifso, ifnot) ->
      let newarg = self#reload_test tst i.arg in
      insert_moves i.arg newarg
        (instr_cons
          (Iifthenelse(tst, self#reload ifso, self#reload ifnot)) newarg [||]
          (self#reload i.next))
  | Iswitch(index, cases) ->
      let newarg = self#makeregs i.arg in
      insert_moves i.arg newarg
        (instr_cons (Iswitch(index, Array.map (self#reload) cases)) newarg [||]
          (self#reload i.next))
  | Iloop body ->
      instr_cons (Iloop(self#reload body)) [||] [||] (self#reload i.next)
  | Icatch(nfail, body, handler) ->
      instr_cons
        (Icatch(nfail, self#reload body, self#reload handler)) [||] [||]
        (self#reload i.next)
  | Iexit i ->
      instr_cons (Iexit i) [||] [||] dummy_instr
  | Itrywith(body, handler) ->
      instr_cons (Itrywith(self#reload body, self#reload handler)) [||] [||]
        (self#reload i.next)

method fundecl f =
  redo_regalloc <- false;
  let new_body = self#reload f.fun_body in
  ({fun_name = f.fun_name; fun_args = f.fun_args;
    fun_body = new_body; fun_fast = f.fun_fast;
    fun_dbg  = f.fun_dbg},
   redo_regalloc)

end
