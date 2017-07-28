(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Gallium, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2014 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Dead code elimination: remove pure instructions whose results are
   not used. *)

open Mach

(* [deadcode i] returns a pair of an optimized instruction [i']
   and a set of registers live "before" instruction [i]. *)

let rec deadcode i =
  match i.desc with
  | Iend | Ireturn | Iop(Itailcall_ind) | Iop(Itailcall_imm _) | Iraise _ ->
      (i, Reg.add_set_array i.live i.arg)
  | Iop op ->
      let (s, before) = deadcode i.next in
      if Proc.op_is_pure op                     (* no side effects *)
      && Reg.disjoint_set_array before i.res    (* results are not used after *)
      && not (Proc.regs_are_volatile i.arg)    (* no stack-like hard reg *)
      && not (Proc.regs_are_volatile i.res)    (*            is involved *)
      then begin
        assert (Array.length i.res > 0);  (* sanity check *)
        (s, before)
      end else begin
        ({i with next = s}, Reg.add_set_array i.live i.arg)
      end
  | Iifthenelse(test, ifso, ifnot) ->
      let (ifso', _) = deadcode ifso in
      let (ifnot', _) = deadcode ifnot in
      let (s, _) = deadcode i.next in
      ({i with desc = Iifthenelse(test, ifso', ifnot'); next = s},
       Reg.add_set_array i.live i.arg)
  | Iswitch(index, cases) ->
      let cases' = Array.map (fun c -> fst (deadcode c)) cases in
      let (s, _) = deadcode i.next in
      ({i with desc = Iswitch(index, cases'); next = s},
       Reg.add_set_array i.live i.arg)
  | Iloop(body) ->
      let (body', _) = deadcode body in
      let (s, _) = deadcode i.next in
      ({i with desc = Iloop body'; next = s}, i.live)
  | Icatch(nfail, body, handler) ->
      let (body', _) = deadcode body in
      let (handler', _) = deadcode handler in
      let (s, _) = deadcode i.next in
      ({i with desc = Icatch(nfail, body', handler'); next = s}, i.live)
  | Iexit nfail ->
      (i, i.live)
  | Itrywith(body, handler) ->
      let (body', _) = deadcode body in
      let (handler', _) = deadcode handler in
      let (s, _) = deadcode i.next in
      ({i with desc = Itrywith(body', handler'); next = s}, i.live)

let fundecl f =
  let (new_body, _) = deadcode f.fun_body in
  {f with fun_body = new_body}
