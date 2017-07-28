(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Combine heap allocations occurring in the same basic block *)

open Mach

type allocation_state =
    No_alloc                            (* no allocation is pending *)
  | Pending_alloc of Reg.t * int        (* an allocation is pending *)
(* The arguments of Pending_alloc(reg, ofs) are:
     reg  the register holding the result of the last allocation
     ofs  the alloc position in the allocated block *)

let allocated_size = function
    No_alloc -> 0
  | Pending_alloc(reg, ofs) -> ofs

let rec combine i allocstate =
  match i.desc with
    Iend | Ireturn | Iexit _ | Iraise _ ->
      (i, allocated_size allocstate)
  | Iop(Ialloc sz) ->
      begin match allocstate with
        No_alloc ->
          let (newnext, newsz) =
            combine i.next (Pending_alloc(i.res.(0), sz)) in
          (instr_cons (Iop(Ialloc newsz)) i.arg i.res newnext, 0)
      | Pending_alloc(reg, ofs) ->
          if ofs + sz < Config.max_young_wosize * Arch.size_addr then begin
            let (newnext, newsz) =
              combine i.next (Pending_alloc(reg, ofs + sz)) in
            (instr_cons (Iop(Iintop_imm(Iadd, ofs))) [| reg |] i.res newnext,
             newsz)
          end else begin
            let (newnext, newsz) =
              combine i.next (Pending_alloc(i.res.(0), sz)) in
            (instr_cons (Iop(Ialloc newsz)) i.arg i.res newnext, ofs)
          end
      end
  | Iop(Icall_ind | Icall_imm _ | Iextcall _ |
        Itailcall_ind | Itailcall_imm _) ->
      let newnext = combine_restart i.next in
      (instr_cons_debug i.desc i.arg i.res i.dbg newnext,
       allocated_size allocstate)
  | Iop op ->
      let (newnext, sz) = combine i.next allocstate in
      (instr_cons_debug i.desc i.arg i.res i.dbg newnext, sz)
  | Iifthenelse(test, ifso, ifnot) ->
      let newifso = combine_restart ifso in
      let newifnot = combine_restart ifnot in
      let newnext = combine_restart i.next in
      (instr_cons (Iifthenelse(test, newifso, newifnot)) i.arg i.res newnext,
       allocated_size allocstate)
  | Iswitch(table, cases) ->
      let newcases = Array.map combine_restart cases in
      let newnext = combine_restart i.next in
      (instr_cons (Iswitch(table, newcases)) i.arg i.res newnext,
       allocated_size allocstate)
  | Iloop(body) ->
      let newbody = combine_restart body in
      (instr_cons (Iloop(newbody)) i.arg i.res i.next,
       allocated_size allocstate)
  | Icatch(io, body, handler) ->
      let (newbody, sz) = combine body allocstate in
      let newhandler = combine_restart handler in
      let newnext = combine_restart i.next in
      (instr_cons (Icatch(io, newbody, newhandler)) i.arg i.res newnext, sz)
  | Itrywith(body, handler) ->
      let (newbody, sz) = combine body allocstate in
      let newhandler = combine_restart handler in
      let newnext = combine_restart i.next in
      (instr_cons (Itrywith(newbody, newhandler)) i.arg i.res newnext, sz)

and combine_restart i =
  let (newi, _) = combine i No_alloc in newi

let fundecl f =
  {f with fun_body = combine_restart f.fun_body}
