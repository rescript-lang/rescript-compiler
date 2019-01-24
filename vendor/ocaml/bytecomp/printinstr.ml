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

(* Pretty-print lists of instructions *)

open Format
open Lambda
open Instruct

let instruction ppf = function
  | Klabel lbl -> fprintf ppf "L%i:" lbl
  | Kacc n -> fprintf ppf "\tacc %i" n
  | Kenvacc n -> fprintf ppf "\tenvacc %i" n
  | Kpush -> fprintf ppf "\tpush"
  | Kpop n -> fprintf ppf "\tpop %i" n
  | Kassign n -> fprintf ppf "\tassign %i" n
  | Kpush_retaddr lbl -> fprintf ppf "\tpush_retaddr L%i" lbl
  | Kapply n -> fprintf ppf "\tapply %i" n
  | Kappterm(n, m) ->
      fprintf ppf "\tappterm %i, %i" n m
  | Kreturn n -> fprintf ppf "\treturn %i" n
  | Krestart -> fprintf ppf "\trestart"
  | Kgrab n -> fprintf ppf "\tgrab %i" n
  | Kclosure(lbl, n) ->
      fprintf ppf "\tclosure L%i, %i" lbl n
  | Kclosurerec(lbls, n) ->
      fprintf ppf "\tclosurerec";
      List.iter (fun lbl -> fprintf ppf " %i" lbl) lbls;
      fprintf ppf ", %i" n
  | Koffsetclosure n -> fprintf ppf "\toffsetclosure %i" n
  | Kgetglobal id -> fprintf ppf "\tgetglobal %a" Ident.print id
  | Ksetglobal id -> fprintf ppf "\tsetglobal %a" Ident.print id
  | Kconst cst ->
      fprintf ppf "@[<10>\tconst@ %a@]" Printlambda.structured_constant cst
  | Kmakeblock(n, m) ->
      fprintf ppf "\tmakeblock %i, %i" n m
  | Kmakefloatblock(n) ->
      fprintf ppf "\tmakefloatblock %i" n
  | Kgetfield n -> fprintf ppf "\tgetfield %i" n
  | Ksetfield n -> fprintf ppf "\tsetfield %i" n
  | Kgetfloatfield n -> fprintf ppf "\tgetfloatfield %i" n
  | Ksetfloatfield n -> fprintf ppf "\tsetfloatfield %i" n
  | Kvectlength -> fprintf ppf "\tvectlength"
  | Kgetvectitem -> fprintf ppf "\tgetvectitem"
  | Ksetvectitem -> fprintf ppf "\tsetvectitem"
  | Kgetstringchar -> fprintf ppf "\tgetstringchar"
  | Ksetstringchar -> fprintf ppf "\tsetstringchar"
  | Kbranch lbl -> fprintf ppf "\tbranch L%i" lbl
  | Kbranchif lbl -> fprintf ppf "\tbranchif L%i" lbl
  | Kbranchifnot lbl -> fprintf ppf "\tbranchifnot L%i" lbl
  | Kstrictbranchif lbl -> fprintf ppf "\tstrictbranchif L%i" lbl
  | Kstrictbranchifnot lbl ->
      fprintf ppf "\tstrictbranchifnot L%i" lbl
  | Kswitch(consts, blocks) ->
      let labels ppf labs =
        Array.iter (fun lbl -> fprintf ppf "@ %i" lbl) labs in
      fprintf ppf "@[<10>\tswitch%a/%a@]" labels consts labels blocks
  | Kboolnot -> fprintf ppf "\tboolnot"
  | Kpushtrap lbl -> fprintf ppf "\tpushtrap L%i" lbl
  | Kpoptrap -> fprintf ppf "\tpoptrap"
  | Kraise k-> fprintf ppf "\t%s" (Lambda.raise_kind k)
  | Kcheck_signals -> fprintf ppf "\tcheck_signals"
  | Kccall(s, n) ->
      fprintf ppf "\tccall %s, %i" s n
  | Knegint -> fprintf ppf "\tnegint"
  | Kaddint -> fprintf ppf "\taddint"
  | Ksubint -> fprintf ppf "\tsubint"
  | Kmulint -> fprintf ppf "\tmulint"
  | Kdivint -> fprintf ppf "\tdivint"
  | Kmodint -> fprintf ppf "\tmodint"
  | Kandint -> fprintf ppf "\tandint"
  | Korint -> fprintf ppf "\torint"
  | Kxorint -> fprintf ppf "\txorint"
  | Klslint -> fprintf ppf "\tlslint"
  | Klsrint -> fprintf ppf "\tlsrint"
  | Kasrint -> fprintf ppf "\tasrint"
  | Kintcomp Ceq -> fprintf ppf "\teqint"
  | Kintcomp Cneq -> fprintf ppf "\tneqint"
  | Kintcomp Clt -> fprintf ppf "\tltint"
  | Kintcomp Cgt -> fprintf ppf "\tgtint"
  | Kintcomp Cle -> fprintf ppf "\tleint"
  | Kintcomp Cge -> fprintf ppf "\tgeint"
  | Koffsetint n -> fprintf ppf "\toffsetint %i" n
  | Koffsetref n -> fprintf ppf "\toffsetref %i" n
  | Kisint -> fprintf ppf "\tisint"
  | Kisout -> fprintf ppf "\tisout"
  | Kgetmethod -> fprintf ppf "\tgetmethod"
  | Kgetpubmet n -> fprintf ppf "\tgetpubmet %i" n
  | Kgetdynmet -> fprintf ppf "\tgetdynmet"
  | Kstop -> fprintf ppf "\tstop"
  | Kevent ev -> fprintf ppf "\tevent \"%s\" %i-%i"
                         ev.ev_loc.Location.loc_start.Lexing.pos_fname
                         ev.ev_loc.Location.loc_start.Lexing.pos_cnum
                         ev.ev_loc.Location.loc_end.Lexing.pos_cnum

let rec instruction_list ppf = function
    [] -> ()
  | Klabel lbl :: il ->
      fprintf ppf "L%i:%a" lbl instruction_list il
  | instr :: il ->
      fprintf ppf "%a@ %a" instruction instr instruction_list il

let instrlist ppf il =
  fprintf ppf "@[<v 0>%a@]" instruction_list il
