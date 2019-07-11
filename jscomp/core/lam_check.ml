(* Copyright (C) 2018 Authors of BuckleScript
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

(** checks 1. variables are not bound twice 2. all variables are of right scope *)
let check file lam =
  let defined_variables = Ident_hash_set.create 1000 in
  let success = ref true in
  let use (id : Ident.t) =
    if not @@ Ident_hash_set.mem defined_variables id then (
      Format.fprintf Format.err_formatter
        "\n[SANITY]:%s/%d used before defined in %s\n" id.name id.stamp file ;
      success := false ) in
  let def (id : Ident.t) =
    if Ident_hash_set.mem defined_variables id then (
      Format.fprintf Format.err_formatter
        "\n[SANITY]:%s/%d bound twice in %s\n" id.name id.stamp file ;
      success := false )
    else Ident_hash_set.add defined_variables id in
  let rec iter_list xs = List.iter iter xs
  and iter_list_snd : 'a. ('a * Lam.t) list -> unit =
   fun xs -> Ext_list.iter_snd xs iter
  and iter (l : Lam.t) =
    match l with
    | Lvar id -> use id
    | Lglobal_module _ -> ()
    | Lprim {args; _} -> iter_list args
    | Lconst _ -> ()
    | Lapply {fn; args; _} -> iter fn ; iter_list args
    | Lfunction {body; params} -> List.iter def params ; iter body
    | Llet (str, id, arg, body) -> iter arg ; def id ; iter body
    | Lletrec (decl, body) ->
        Ext_list.iter_fst decl def ; iter_list_snd decl ; iter body
    | Lswitch (arg, sw) ->
        iter arg ;
        iter_list_snd sw.sw_consts ;
        iter_list_snd sw.sw_blocks ;
        Ext_option.iter sw.sw_failaction iter ;
        assert (
          not (sw.sw_failaction <> None && sw.sw_numconsts && sw.sw_numblocks)
        )
    | Lstringswitch (arg, cases, default) ->
        iter arg ;
        iter_list_snd cases ;
        Ext_option.iter default iter
    | Lstaticraise (_i, args) -> iter_list args
    | Lstaticcatch (e1, (_, vars), e2) ->
        iter e1 ; List.iter def vars ; iter e2
    | Ltrywith (e1, exn, e2) -> iter e1 ; def exn ; iter e2
    | Lifthenelse (e1, e2, e3) -> iter e1 ; iter e2 ; iter e3
    | Lsequence (e1, e2) -> iter e1 ; iter e2
    | Lwhile (e1, e2) -> iter e1 ; iter e2
    | Lfor (v, e1, e2, dir, e3) -> iter e1 ; iter e2 ; def v ; iter e3
    | Lassign (id, e) -> use id ; iter e
    | Lsend (k, met, obj, args, _) -> iter met ; iter obj ; iter_list args
  in
  iter lam ;
  assert !success ;
  lam
