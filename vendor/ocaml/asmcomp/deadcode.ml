(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Gallium, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2014 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Dead code elimination: remove pure instructions whose results are
   not used. *)

open Mach

(* [deadcode i] returns a pair of an optimized instruction [i']
   and a set of registers live "before" instruction [i]. *)

let rec deadcode i =
  let arg =
    if Config.spacetime
      && Mach.spacetime_node_hole_pointer_is_live_before i
    then Array.append i.arg [| Proc.loc_spacetime_node_hole |]
    else i.arg
  in
  match i.desc with
  | Iend | Ireturn | Iop(Itailcall_ind _) | Iop(Itailcall_imm _) | Iraise _ ->
      (i, Reg.add_set_array i.live arg)
  | Iop op ->
      let (s, before) = deadcode i.next in
      if Proc.op_is_pure op                     (* no side effects *)
      && Reg.disjoint_set_array before i.res    (* results are not used after *)
      && not (Proc.regs_are_volatile arg)      (* no stack-like hard reg *)
      && not (Proc.regs_are_volatile i.res)    (*            is involved *)
      then begin
        assert (Array.length i.res > 0);  (* sanity check *)
        (s, before)
      end else begin
        ({i with next = s}, Reg.add_set_array i.live arg)
      end
  | Iifthenelse(test, ifso, ifnot) ->
      let (ifso', _) = deadcode ifso in
      let (ifnot', _) = deadcode ifnot in
      let (s, _) = deadcode i.next in
      ({i with desc = Iifthenelse(test, ifso', ifnot'); next = s},
       Reg.add_set_array i.live arg)
  | Iswitch(index, cases) ->
      let cases' = Array.map (fun c -> fst (deadcode c)) cases in
      let (s, _) = deadcode i.next in
      ({i with desc = Iswitch(index, cases'); next = s},
       Reg.add_set_array i.live arg)
  | Iloop(body) ->
      let (body', _) = deadcode body in
      let (s, _) = deadcode i.next in
      ({i with desc = Iloop body'; next = s}, i.live)
  | Icatch(rec_flag, handlers, body) ->
      let (body', _) = deadcode body in
      let handlers' =
        List.map (fun (nfail, handler) ->
            let (handler', _) = deadcode handler in
            nfail, handler')
          handlers
      in
      let (s, _) = deadcode i.next in
      ({i with desc = Icatch(rec_flag, handlers', body'); next = s}, i.live)
  | Iexit _nfail ->
      (i, i.live)
  | Itrywith(body, handler) ->
      let (body', _) = deadcode body in
      let (handler', _) = deadcode handler in
      let (s, _) = deadcode i.next in
      ({i with desc = Itrywith(body', handler'); next = s}, i.live)

let fundecl f =
  let (new_body, _) = deadcode f.fun_body in
  {f with fun_body = new_body}
