(* OCamlScript compiler
 * Copyright (C) 2015-2016 Bloomberg Finance L.P.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

(* Author: Hongbo Zhang  *)



(*
   [map] 
   and 
   ATTENTION: [let] bound idents have to be renamed, 
   note we rely on an invariant that parameter could not be rebound 
 *)
let rename 
    (map :   (Ident.t, Ident.t) Hashtbl.t) 
    (lam : Lambda.lambda) : Lambda.lambda = 

  let rebind i = 
    let i' = Ident.rename i in 
    Hashtbl.add map i i';
    i' in

  let rec aux (lam : Lambda.lambda) : Lambda.lambda = 
    match lam with 
    | Lvar v -> 
      (try Lvar (Hashtbl.find map v) with Not_found -> lam)
    | Lconst _ -> lam
    | Llet(str, v, l1, l2) ->
      let v = rebind v in
      Llet(str, v, aux l1, aux l2 )
    | Lletrec(bindings, body) ->
      let bindings = 
        List.map (fun (k,l) ->  
            let k = rebind k in (* order matters *)
            (k, aux l)
          ) bindings in 
      Lletrec(bindings, aux body) 
    | Lprim(prim, ll) -> Lprim(prim, List.map aux  ll)
    | Lapply(l1, ll, info) ->
      Lapply(aux  l1, List.map aux  ll,info)
    | Lfunction(kind, params, l) -> 
      let params =  List.map rebind params in
      Lfunction (kind, params, aux  l)
    | Lswitch(l, {sw_failaction; 
                  sw_consts; 
                  sw_blocks;
                  sw_numblocks;
                  sw_numconsts;
                 }) ->
      let l = aux l in
      Lswitch(l,
              {sw_consts = 
                 List.map (fun (v, l) -> v, aux  l) sw_consts;
               sw_blocks = List.map (fun (v, l) -> v, aux  l) sw_blocks;
               sw_numconsts = sw_numconsts;
               sw_numblocks = sw_numblocks;
               sw_failaction = 
                 begin 
                   match sw_failaction with 
                   | None -> None
                   | Some x -> Some (aux x)
                 end})
    | Lstringswitch(l, sw, d) ->
      let l = aux  l in
      Lstringswitch( l ,
                     List.map (fun (i, l) -> i,aux  l) sw,
                     begin 
                       match d with
                       | Some d -> Some (aux d )
                       | None -> None
                     end)
    | Lstaticraise (i,ls) -> Lstaticraise(i, List.map aux  ls)
    | Lstaticcatch(l1, (i,xs), l2) -> 
      let l1 = aux l1 in
      let xs = List.map rebind xs in
      let l2 = aux l2 in
      Lstaticcatch(l1, (i,xs), l2)
    | Ltrywith(l1, v, l2) -> 
      let l1 = aux l1 in
      let v = rebind v in
      let l2 = aux l2 in
      Ltrywith(l1,v, l2)
    | Lifthenelse(l1, l2, l3) -> 
      let l1 = aux l1 in
      let l2 = aux l2 in
      let l3 = aux l3 in
      Lifthenelse(l1,  l2,   l3)
    | Lsequence(l1, l2) -> 
      let l1 = aux l1 in
      let l2 = aux l2 in
      Lsequence(  l1,   l2)
    | Lwhile(l1, l2) -> 
      let l1 = aux l1 in
      let l2 = aux l2 in
      Lwhile(  l1,  l2)
    | Lfor(ident, l1, l2, dir, l3) ->
      let ident = rebind ident in 
      let l1 = aux l1 in
      let l2 = aux l2 in
      let l3 = aux l3 in
      Lfor(ident,aux  l1,  l2, dir,  l3)
    | Lassign(v, l) ->
      (* Lalias-bound variables are never assigned, so don't increase
         v's refsimpl *)
      Lassign(v,aux  l)
    | Lsend(u, m, o, ll, v) ->
      let m = aux m in 
      let o = aux o in 
      let ll = List.map aux ll in
      Lsend(u,  m,  o,  ll,v)
    | Levent(l, event) ->
      let l = aux l in
      Levent(  l, event)
    | Lifused(v, l) -> 
      let l = aux l in 
      Lifused(v,  l) in 
  aux lam

let rec bounded_idents tbl lam = 
  let rebind i = Hashtbl.add tbl i (Ident.rename i) in
  let rec collect_introduced_idents  (lam : Lambda.lambda) = 
    match lam with 
    | Lvar  _ 
    | Lconst _ -> ()
    | Lapply ( f , ls, _) -> 
        collect_introduced_idents f ; 
        List.iter collect_introduced_idents ls 
    | Lfunction (_, args, lam) -> 
        List.iter (fun a -> rebind a) args; 
        collect_introduced_idents lam
    | Llet (_, id,arg,body) -> 
        rebind id; 
        collect_introduced_idents arg ; 
        collect_introduced_idents body ; 
    | Lletrec (bindings, body) -> 
        List.iter (fun (i,arg) -> 
          rebind i;
          collect_introduced_idents arg) bindings;
        collect_introduced_idents body
    | Lprim (_, lams) -> 
        List.iter collect_introduced_idents lams 
    | Lswitch (lam ,switch) -> 
        collect_introduced_idents lam ; 
        assert false 
   (* switch on strings, clauses are sorted by string order,
      strings are pairwise distinct *)
    | Lstringswitch _ -> assert false 
    | Lstaticraise (_,ls) -> List.iter collect_introduced_idents ls 
    | Lstaticcatch (lam, (_i, is), body) ->  
        (** Note that [staticcatch] does not introduce idents?
            double check?
         *)
        List.iter rebind is;
        collect_introduced_idents lam; 
        collect_introduced_idents body 
    | Ltrywith (a,i,b) -> 
        rebind i;
        collect_introduced_idents a; 
        collect_introduced_idents b 
    | Lifthenelse (a,b,c) -> 
        collect_introduced_idents a ; 
        collect_introduced_idents b ; 
        collect_introduced_idents c
    | Lsequence (a,b) -> 
        collect_introduced_idents a; 
        collect_introduced_idents b ; 
    | Lwhile (a,b) -> 
        collect_introduced_idents a; 
        collect_introduced_idents b ; 
    | Lfor (i, a,b,_direction,l) -> 
        rebind i;
        collect_introduced_idents a ; 
        collect_introduced_idents b ; 
        collect_introduced_idents l ; 
    | Lassign (_v, a) ->       collect_introduced_idents a 
    | Lsend (_, a,b,ls, _location) -> 
        collect_introduced_idents a ; 
        collect_introduced_idents b ; 
        List.iter collect_introduced_idents ls 
    | Levent (a,_event) -> 
        collect_introduced_idents a 
    | Lifused (_id,a ) ->
        collect_introduced_idents a 
  in 
  collect_introduced_idents lam

let refresh_lambda (lam : Lambda.lambda ) : Lambda.lambda  = 
  let map = Hashtbl.create 57 in 
  begin
    bounded_idents map lam ;
    rename map lam
  end

(* 
    A naive beta reduce would break the invariants of the optmization.


    The sane but slowest  way:
      when we do a beta reduction, we need rename all variables inlcuding 
      let-bound ones

    A conservative one:
      - for internal one 
        rename params and let bound variables
      - for external one (seriaized)
        if it's enclosed environment should be good enough
        so far, we only inline enclosed lambdas
    TODO: rename 
 *)
let propogate_beta_reduce 
    (meta : Lam_stats.meta) params body args =
  let new_params = 
    List.map Ident.rename params in 
  let map = Hashtbl.create 51 in
  let () = 
    begin 
      List.iter2 (fun k v -> 
          Hashtbl.add map k v )
        params new_params ;
    end
  in

  let new_body = rename map body in
  let lam = List.fold_left2 
      (fun l param (arg : Lambda.lambda) -> 
         begin 
           let arg = 
             (
               match arg with 
               | Lvar v -> 
                 begin 
                   match Hashtbl.find meta.ident_tbl v with 
                   | exception Not_found -> 
                     (* Ext_log.err "@[%a ++ @]@." Ident.print param; *)
                     ()
                   | ident_info -> 
                     Hashtbl.add meta.ident_tbl param ident_info 
                 end;
                 arg 
               | Lprim (Pgetglobal ident, []) -> 
                 (* It's not completeness, its to make it sound.. *)
                 Lam_compile_global.query_lambda ident meta.env 
               (* alias meta param ident (Module (Global ident)) Strict *)
               | Lprim (Pmakeblock (_, _, Immutable ) , ls) -> 
                 Hashtbl.replace meta.ident_tbl param 
                   (Lam_util.kind_of_lambda_block ls ); (** *)
                 arg
               | _ -> 
                 begin 
                   (* Ext_log.err "@[%a -- %a @]@."  *)
                   (*   Ident.print param  *)
                   (*   Printlambda.lambda arg *)
                   arg
                 end
             ) in
           (** TODO: refactor mklet 
               and to be improved later
           *)
           Lam_util.refine_let param arg l
         end
      ) 
      new_body new_params args in
  (* let lam = Lam_util.deep_flatten  lam in *)
  (* Lam_pass_collect.collect_helper meta lam;  *)
  lam


let beta_reduce params body args =
  List.fold_left2 
    (fun l param arg ->
       Lam_util.refine_let param arg l)
    body params args
