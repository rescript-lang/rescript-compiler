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

(* Pretty-printing of linearized machine code *)

open Format
open Mach
open Printmach
open Linearize

let label ppf l =
  Format.fprintf ppf "L%i" l

let instr ppf i =
  begin match i.desc with
  | Lend -> ()
  | Lop op ->
      begin match op with
      | Ialloc _ | Icall_ind | Icall_imm _ | Iextcall(_, _) ->
          fprintf ppf "@[<1>{%a}@]@," regsetaddr i.live
      | _ -> ()
      end;
      operation op i.arg ppf i.res
  | Lreloadretaddr ->
      fprintf ppf "reload retaddr"
  | Lreturn ->
      fprintf ppf "return %a" regs i.arg
  | Llabel lbl ->
      fprintf ppf "%a:" label lbl
  | Lbranch lbl ->
      fprintf ppf "goto %a" label lbl
  | Lcondbranch(tst, lbl) ->
      fprintf ppf "if %a goto %a" (test tst) i.arg label lbl
  | Lcondbranch3(lbl0, lbl1, lbl2) ->
      fprintf ppf "switch3 %a" reg i.arg.(0);
      let case n = function
      | None -> ()
      | Some lbl ->
         fprintf ppf "@,case %i: goto %a" n label lbl in
      case 0 lbl0; case 1 lbl1; case 2 lbl2;
      fprintf ppf "@,endswitch"
  | Lswitch lblv ->
      fprintf ppf "switch %a" reg i.arg.(0);
      for i = 0 to Array.length lblv - 1 do
       fprintf ppf "case %i: goto %a" i label lblv.(i)
      done;
      fprintf ppf "@,endswitch"
  | Lsetuptrap lbl ->
      fprintf ppf "setup trap %a" label lbl
  | Lpushtrap ->
      fprintf ppf "push trap"
  | Lpoptrap ->
      fprintf ppf "pop trap"
  | Lraise k ->
      fprintf ppf "%s %a" (Lambda.raise_kind k) reg i.arg.(0)
  end;
  if not (Debuginfo.is_none i.dbg) then
    fprintf ppf " %s" (Debuginfo.to_string i.dbg)

let rec all_instr ppf i =
  match i.desc with
  | Lend -> ()
  | _ -> fprintf ppf "%a@,%a" instr i all_instr i.next

let fundecl ppf f =
  let dbg =
    if Debuginfo.is_none f.fun_dbg then
      ""
    else
      " " ^ Debuginfo.to_string f.fun_dbg in
  fprintf ppf "@[<v 2>%s:%s@,%a@]" f.fun_name dbg all_instr f.fun_body
