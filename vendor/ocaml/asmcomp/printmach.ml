(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Pretty-printing of pseudo machine code *)

open Format
open Cmm
open Reg
open Mach
open Interval

let reg ppf r =
  if not (Reg.anonymous r) then
    fprintf ppf "%s" (Reg.name r)
  else
    fprintf ppf "%s"
      (match r.typ with Val -> "V" | Addr -> "A" | Int -> "I" | Float -> "F");
  fprintf ppf "/%i" r.stamp;
  begin match r.loc with
  | Unknown -> ()
  | Reg r ->
      fprintf ppf "[%s]" (Proc.register_name r)
  | Stack(Local s) ->
      fprintf ppf "[s%i]" s
  | Stack(Incoming s) ->
      fprintf ppf "[si%i]" s
  | Stack(Outgoing s) ->
      fprintf ppf "[so%i]" s
  end

let regs ppf v =
  match Array.length v with
  | 0 -> ()
  | 1 -> reg ppf v.(0)
  | n -> reg ppf v.(0);
         for i = 1 to n-1 do fprintf ppf " %a" reg v.(i) done

let regset ppf s =
  let first = ref true in
  Reg.Set.iter
    (fun r ->
      if !first then begin first := false; fprintf ppf "%a" reg r end
      else fprintf ppf "@ %a" reg r)
    s

let regsetaddr ppf s =
  let first = ref true in
  Reg.Set.iter
    (fun r ->
      if !first then begin first := false; fprintf ppf "%a" reg r end
      else fprintf ppf "@ %a" reg r;
      match r.typ with
      | Val -> fprintf ppf "*"
      | Addr -> fprintf ppf "!"
      | _ -> ())
    s

let intcomp = function
  | Isigned c -> Printf.sprintf " %ss " (Printcmm.comparison c)
  | Iunsigned c -> Printf.sprintf " %su " (Printcmm.comparison c)

let floatcomp c =
    Printf.sprintf " %sf " (Printcmm.comparison c)

let intop = function
  | Iadd -> " + "
  | Isub -> " - "
  | Imul -> " * "
  | Imulh -> " *h "
  | Idiv -> " div "
  | Imod -> " mod "
  | Iand -> " & "
  | Ior ->  " | "
  | Ixor -> " ^ "
  | Ilsl -> " << "
  | Ilsr -> " >>u "
  | Iasr -> " >>s "
  | Icomp cmp -> intcomp cmp
  | Icheckbound { label_after_error; spacetime_index; } ->
    if not Config.spacetime then " check > "
    else
      Printf.sprintf "check[lbl=%s,index=%d] > "
        begin
          match label_after_error with
          | None -> ""
          | Some lbl -> string_of_int lbl
        end
        spacetime_index

let test tst ppf arg =
  match tst with
  | Itruetest -> reg ppf arg.(0)
  | Ifalsetest -> fprintf ppf "not %a" reg arg.(0)
  | Iinttest cmp -> fprintf ppf "%a%s%a" reg arg.(0) (intcomp cmp) reg arg.(1)
  | Iinttest_imm(cmp, n) -> fprintf ppf "%a%s%i" reg arg.(0) (intcomp cmp) n
  | Ifloattest(cmp, neg) ->
      fprintf ppf "%s%a%s%a"
       (if neg then "not " else "")
       reg arg.(0) (floatcomp cmp) reg arg.(1)
  | Ieventest -> fprintf ppf "%a & 1 == 0" reg arg.(0)
  | Ioddtest -> fprintf ppf "%a & 1 == 1" reg arg.(0)

let print_live = ref false

let operation op arg ppf res =
  if Array.length res > 0 then fprintf ppf "%a := " regs res;
  match op with
  | Imove -> regs ppf arg
  | Ispill -> fprintf ppf "%a (spill)" regs arg
  | Ireload -> fprintf ppf "%a (reload)" regs arg
  | Iconst_int n -> fprintf ppf "%s" (Nativeint.to_string n)
  | Iconst_float f -> fprintf ppf "%F" (Int64.float_of_bits f)
  | Iconst_symbol s -> fprintf ppf "\"%s\"" s
  | Icall_ind _ -> fprintf ppf "call %a" regs arg
  | Icall_imm { func; _ } -> fprintf ppf "call \"%s\" %a" func regs arg
  | Itailcall_ind _ -> fprintf ppf "tailcall %a" regs arg
  | Itailcall_imm { func; } -> fprintf ppf "tailcall \"%s\" %a" func regs arg
  | Iextcall { func; alloc; _ } ->
      fprintf ppf "extcall \"%s\" %a%s" func regs arg
      (if alloc then "" else " (noalloc)")
  | Istackoffset n ->
      fprintf ppf "offset stack %i" n
  | Iload(chunk, addr) ->
      fprintf ppf "%s[%a]"
       (Printcmm.chunk chunk) (Arch.print_addressing reg addr) arg
  | Istore(chunk, addr, is_assign) ->
      fprintf ppf "%s[%a] := %a %s"
       (Printcmm.chunk chunk)
       (Arch.print_addressing reg addr)
       (Array.sub arg 1 (Array.length arg - 1))
       reg arg.(0)
       (if is_assign then "(assign)" else "(init)")
  | Ialloc { words = n; _ } ->
    fprintf ppf "alloc %i" n;
    if Config.spacetime then begin
      fprintf ppf "(spacetime node = %a)" reg arg.(0)
    end
  | Iintop(op) -> fprintf ppf "%a%s%a" reg arg.(0) (intop op) reg arg.(1)
  | Iintop_imm(op, n) -> fprintf ppf "%a%s%i" reg arg.(0) (intop op) n
  | Inegf -> fprintf ppf "-f %a" reg arg.(0)
  | Iabsf -> fprintf ppf "absf %a" reg arg.(0)
  | Iaddf -> fprintf ppf "%a +f %a" reg arg.(0) reg arg.(1)
  | Isubf -> fprintf ppf "%a -f %a" reg arg.(0) reg arg.(1)
  | Imulf -> fprintf ppf "%a *f %a" reg arg.(0) reg arg.(1)
  | Idivf -> fprintf ppf "%a /f %a" reg arg.(0) reg arg.(1)
  | Ifloatofint -> fprintf ppf "floatofint %a" reg arg.(0)
  | Iintoffloat -> fprintf ppf "intoffloat %a" reg arg.(0)
  | Iname_for_debugger { ident; which_parameter; } ->
    fprintf ppf "name_for_debugger %a%s=%a"
      Ident.print ident
      (match which_parameter with
        | None -> ""
        | Some index -> sprintf "[P%d]" index)
      reg arg.(0)
  | Ispecific op ->
      Arch.print_specific_operation reg op ppf arg

let rec instr ppf i =
  if !print_live then begin
    fprintf ppf "@[<1>{%a" regsetaddr i.live;
    if Array.length i.arg > 0 then fprintf ppf "@ +@ %a" regs i.arg;
    fprintf ppf "}@]@,";
    if !Clflags.dump_avail then begin
      let module RAS = Reg_availability_set in
      fprintf ppf "@[<1>AB={%a}" (RAS.print ~print_reg:reg) i.available_before;
      begin match i.available_across with
      | None -> ()
      | Some available_across ->
        fprintf ppf ",AA={%a}" (RAS.print ~print_reg:reg) available_across
      end;
      fprintf ppf "@]@,"
    end
  end;
  begin match i.desc with
  | Iend -> ()
  | Iop op ->
      operation op i.arg ppf i.res
  | Ireturn ->
      fprintf ppf "return %a" regs i.arg
  | Iifthenelse(tst, ifso, ifnot) ->
      fprintf ppf "@[<v 2>if %a then@,%a" (test tst) i.arg instr ifso;
      begin match ifnot.desc with
      | Iend -> ()
      | _ -> fprintf ppf "@;<0 -2>else@,%a" instr ifnot
      end;
      fprintf ppf "@;<0 -2>endif@]"
  | Iswitch(index, cases) ->
      fprintf ppf "switch %a" reg i.arg.(0);
      for i = 0 to Array.length cases - 1 do
        fprintf ppf "@,@[<v 2>@[";
        for j = 0 to Array.length index - 1 do
          if index.(j) = i then fprintf ppf "case %i:@," j
        done;
        fprintf ppf "@]@,%a@]" instr cases.(i)
      done;
      fprintf ppf "@,endswitch"
  | Iloop(body) ->
      fprintf ppf "@[<v 2>loop@,%a@;<0 -2>endloop@]" instr body
  | Icatch(flag, handlers, body) ->
      fprintf ppf "@[<v 2>catch%a@,%a@;<0 -2>with"
        Printcmm.rec_flag flag instr body;
      let h (nfail, handler) =
        fprintf ppf "(%d)@,%a@;" nfail instr handler in
      let rec aux = function
        | [] -> ()
        | [v] -> h v
        | v :: t ->
            h v;
            fprintf ppf "@ and";
            aux t
      in
      aux handlers
  | Iexit i ->
      fprintf ppf "exit(%d)" i
  | Itrywith(body, handler) ->
      fprintf ppf "@[<v 2>try@,%a@;<0 -2>with@,%a@;<0 -2>endtry@]"
             instr body instr handler
  | Iraise k ->
      fprintf ppf "%a %a" Printcmm.raise_kind k reg i.arg.(0)
  end;
  if not (Debuginfo.is_none i.dbg) then
    fprintf ppf "%s" (Debuginfo.to_string i.dbg);
  begin match i.next.desc with
    Iend -> ()
  | _ -> fprintf ppf "@,%a" instr i.next
  end

let fundecl ppf f =
  let dbg =
    if Debuginfo.is_none f.fun_dbg then
      ""
    else
      " " ^ Debuginfo.to_string f.fun_dbg in
  fprintf ppf "@[<v 2>%s(%a)%s@,%a@]"
    f.fun_name regs f.fun_args dbg instr f.fun_body

let phase msg ppf f =
  fprintf ppf "*** %s@.%a@." msg fundecl f

let interference ppf r =
  let interf ppf =
   List.iter
    (fun r -> fprintf ppf "@ %a" reg r)
    r.interf in
  fprintf ppf "@[<2>%a:%t@]@." reg r interf

let interferences ppf () =
  fprintf ppf "*** Interferences@.";
  List.iter (interference ppf) (Reg.all_registers())

let interval ppf i =
  let interv ppf =
    List.iter
      (fun r -> fprintf ppf "@ [%d;%d]" r.rbegin r.rend)
      i.ranges in
  fprintf ppf "@[<2>%a:%t@]@." reg i.reg interv

let intervals ppf () =
  fprintf ppf "*** Intervals@.";
  List.iter (interval ppf) (Interval.all_fixed_intervals());
  List.iter (interval ppf) (Interval.all_intervals())

let preference ppf r =
  let prefs ppf =
    List.iter
      (fun (r, w) -> fprintf ppf "@ %a weight %i" reg r w)
      r.prefer in
  fprintf ppf "@[<2>%a: %t@]@." reg r prefs

let preferences ppf () =
  fprintf ppf "*** Preferences@.";
  List.iter (preference ppf) (Reg.all_registers())
