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


 let pass_free_variables (l : Lam.t) : Ident_set.t =
  let fv = ref Ident_set.empty in
  let rec free (l : Lam.t) =
    begin
      match l with
      | Lvar id -> fv := Ident_set.add id !fv
      | Lassign(id, e) ->
        free e;
        fv := Ident_set.add id !fv
      | Lstaticcatch(e1, (_,vars), e2) ->
        free e1; free e2;
        List.iter (fun id -> fv := Ident_set.remove id !fv) vars
      | Ltrywith(e1, exn, e2) ->
        free e1; free e2;
        fv := Ident_set.remove exn !fv
      | Lfunction{body;params} ->
        free body;
        List.iter (fun param -> fv := Ident_set.remove param !fv) params
      | Llet(str, id, arg, body) ->
        free arg; free body;
        fv := Ident_set.remove id !fv
      | Lletrec(decl, body) ->
        free body;
        List.iter (fun (id, exp) -> free exp) decl;
        List.iter (fun (id, exp) -> fv := Ident_set.remove id !fv) decl
      | Lfor(v, e1, e2, dir, e3) ->
        free e1; free e2; free e3;
        fv := Ident_set.remove v !fv
      | Lconst _ -> ()
      | Lapply{fn; args; _} ->
        free fn; List.iter free args
      | Lglobal_module _ -> ()
      (* according to the existing semantics:
         [primitive] is not counted
      *)
      | Lprim {args; _} ->
        List.iter free args
      | Lswitch(arg, sw) ->
        free arg;
        List.iter (fun (key, case) -> free case) sw.sw_consts;
        List.iter (fun (key, case) -> free case) sw.sw_blocks;
        begin match sw.sw_failaction with
          | None -> ()
          | Some a -> free a
        end
      | Lstringswitch (arg,cases,default) ->
        free arg ;
        List.iter (fun (_,act) -> free act) cases ;
        begin match default with
          | None -> ()
          | Some a -> free a
        end
      | Lstaticraise (_,args) ->
        List.iter free args
      | Lifthenelse(e1, e2, e3) ->
        free e1; free e2; free e3
      | Lsequence(e1, e2) ->
        free e1; free e2
      | Lwhile(e1, e2) ->
        free e1; free e2
      | Lsend (k, met, obj, args, _) ->
        free met; free obj; List.iter free args
    end;
  in free l;
  !fv

(*
let add_list lst set =
    List.fold_left (fun acc x -> Ident_set.add x acc) set lst
let free_variables l =
  let rec free bounded acc (l : t) =
      match (l : t) with
      | Lvar id ->
        if Ident_set.mem id bounded then acc
        else Ident_set.add id acc
      | Lconst _ -> acc
      | Lapply{fn; args; _} ->
        let acc = free bounded  acc fn in
        List.fold_left (fun acc arg -> free bounded acc arg) acc args
      | Lfunction{body;params} ->
        let bounded = add_list params bounded in
        free bounded acc  body
      | Llet(str, id, arg, body) ->
        let acc = free bounded acc  arg in
        let bounded =  Ident_set.add id bounded in
        free bounded acc body
      | Lletrec(decl, body) ->
        let bounded =
          List.fold_left (fun acc (x,_) -> Ident_set.add x acc) bounded decl
        in
        let acc = List.fold_left (fun acc (_,exp) -> free bounded acc exp ) acc decl in
        free bounded acc body
      | Lprim {args; _} ->
        List.fold_left (fun acc arg -> free bounded acc arg) acc args
      | Lswitch(arg, {sw_consts; sw_blocks; sw_failaction}) ->
        let acc = free bounded acc arg in
        let acc = List.fold_left
          (fun acc (key, case) -> free  bounded acc case) acc sw_consts in
        let acc =
          List.fold_left
          (fun acc (key, case) -> free bounded acc  case) acc sw_blocks in
        begin match sw_failaction with
          | None -> acc
          | Some a -> free bounded acc a
        end
      | Lstringswitch (arg,cases,default) ->
        let acc = free bounded acc arg  in
        let acc = List.fold_left (fun acc  (_,act) -> free bounded acc act) acc cases  in
        begin match default with
          | None -> acc
          | Some a -> free bounded acc a
        end
      | Lstaticraise (_,args) ->
        List.fold_left (fun acc arg -> free bounded acc arg) acc args
      | Lstaticcatch(e1, (_,vars), e2) ->
        let acc = free  bounded acc e1 in
        let bounded = add_list vars bounded in
        free bounded acc e2
      | Ltrywith(e1, exn, e2) ->
        let acc = free  bounded acc e1 in
        let bounded = Ident_set.add exn bounded in
        free  bounded acc e2
      | Lifthenelse(e1, e2, e3) ->
        let acc = free  bounded acc e1 in
        let acc = free  bounded acc e2 in
        free bounded acc e3
      | Lwhile(e1, e2)
      | Lsequence(e1, e2) ->
        let acc = free bounded acc e1 in
        free bounded acc e2
      | Lfor(v, e1, e2, dir, e3) ->

        let acc = free  bounded acc e1 in
        let acc = free  bounded acc e2 in
        let bounded = Ident_set.add v bounded in
        free bounded acc e3
      | Lassign(id, e) ->
        let acc = free bounded acc  e in
        if Ident_set.mem id bounded then acc
        else Ident_set.add id acc
      | Lsend (k, met, obj, args, _) ->
        let acc = free bounded acc met in
        let acc = free bounded acc obj in
        List.fold_left (fun ac arg -> free bounded acc arg) acc args
      | Lifused (v, e) ->
        free bounded acc e
  in free Ident_set.empty Ident_set.empty l
*)

(**
        [hit_any_variables fv l]
        check the lambda expression [l] if has some free
        variables captured by [fv].
        Note it does not do any checking like below
        [Llet(str,id,arg,body)]
        it only check [arg] or [body] is hit or not, there
        is a case that [id] is hit in [arg] but also exists
        in [fv], this is ignored.
*)