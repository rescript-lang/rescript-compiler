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
(* Adapted for Javascript backend : Hongbo Zhang,  *)

(*A naive dead code elimination *)
type used_info = { 
  mutable times : int ; 
  mutable captured : bool;
  (* captured in functon or loop, 
     inline in such cases should be careful
     1. can not inline mutable values
     2. avoid re-computation 
  *)
}

type occ_tbl  = used_info Ident_hashtbl.t
(* First pass: count the occurrences of all let-bound identifiers *)

type local_tbl = used_info  Ident_map.t

let dummy_info () = {times =  0 ; captured = false }
(* y is untouched *)

let absorb_info (x : used_info) (y : used_info) = 
  match x, y with
  | {times = x0} , {times = y0; captured } -> 
    x.times <- x0 + y0;
    if captured then x.captured <- true


(* The global table [occ] associates to each let-bound identifier
   the number of its uses (as a reference):
   - 0 if never used
   - 1 if used exactly once in and not under a lambda or within a loop
       - when under a lambda, 
       - it's probably a closure
       - within a loop
       - update reference,
       niether is good for inlining
   - > 1 if used several times or under a lambda or within a loop.
   The local table [bv] associates to each locally-let-bound variable
   its reference count, as above.  [bv] is enriched at let bindings
   but emptied when crossing lambdas and loops. *)
let collect_occurs  lam : occ_tbl =
  let occ : occ_tbl = Ident_hashtbl.create 83 in

  (* Current use count of a variable. *)
  let used v = 
    match Ident_hashtbl.find_opt occ v with 
    | None -> false 
    | Some {times ; _} -> times > 0  in

  (* Entering a [let].  Returns updated [bv]. *)
  let bind_var bv ident =
    let r = dummy_info () in
    Ident_hashtbl.add occ ident r;
    Ident_map.add ident r bv in

  (* Record a use of a variable *)
  let add_one_use bv ident  =
    match Ident_map.find_opt ident bv with 
    | Some r  -> r.times <- r.times + 1 
    | None ->
      (* ident is not locally bound, therefore this is a use under a lambda
         or within a loop.  Increase use count by 2 -- enough so
         that single-use optimizations will not apply. *)
      match Ident_hashtbl.find_opt occ ident with 
      | Some r -> absorb_info r {times = 1; captured =  true}
      | None ->
        (* Not a let-bound variable, ignore *)
        () in

  let inherit_use bv ident bid =
    let n =
      match Ident_hashtbl.find_opt occ bid with
      | None -> dummy_info ()
      | Some v -> v in
    match Ident_map.find_opt ident bv with 
    | Some r  -> absorb_info r n
    | None ->
      (* ident is not locally bound, therefore this is a use under a lambda
         or within a loop.  Increase use count by 2 -- enough so
         that single-use optimizations will not apply. *)
      match Ident_hashtbl.find_opt occ ident with 
      | Some r -> absorb_info r {n with captured = true} 
      | None ->
        (* Not a let-bound variable, ignore *)
        () in

  let rec count (bv : local_tbl) (lam : Lam.t) = 
    match lam with 
    | Lfunction{body = l} ->
      count Ident_map.empty l
    (** when entering a function local [bv] 
        is cleaned up, so that all closure variables will not be
        carried over, since the parameters are never rebound, 
        so it is fine to kep it empty
    *)
    | Lfor(_, l1, l2, dir, l3) -> 
      count bv l1;
      count bv l2; 
      count Ident_map.empty l3
    | Lwhile(l1, l2) -> count Ident_map.empty l1; count Ident_map.empty l2
    | Lvar v ->
      add_one_use bv v 
    | Llet(_, v, Lvar w, l2)  ->
      (* v will be replaced by w in l2, so each occurrence of v in l2
         increases w's refcount *)
      count (bind_var bv v) l2;
      inherit_use bv w v 
    | Llet(kind, v, l1, l2) ->
      count (bind_var bv v) l2;
      (* count [l2] first,
         If v is unused, l1 will be removed, so don't count its variables *)
      if kind = Strict || used v then count bv l1
    | Lassign(_, l) ->
      (* Lalias-bound variables are never assigned, so don't increase
         this ident's refcount *)
      count bv l
    | Lglobal_module _ -> ()
    | Lprim {args; _} -> List.iter (count bv ) args
    | Lletrec(bindings, body) ->
      List.iter (fun (v, l) -> count bv l) bindings;
      count bv body
        (** Note there is a difference here when do beta reduction for *)
    | Lapply{fn = Lfunction{function_kind= Curried; params; body};  args; _}
      when  Ext_list.same_length params args ->
      count bv (Lam_beta_reduce.beta_reduce  params body args)
    | Lapply{fn = Lfunction{function_kind = Tupled; params; body};
             args = [Lprim {primitive = Pmakeblock _;  args; _}]; _}
      when  Ext_list.same_length params  args ->
      count bv (Lam_beta_reduce.beta_reduce   params body args)
    | Lapply{fn = l1; args= ll; _} ->
      count bv l1; List.iter (count bv) ll 
    | Lconst cst -> ()
    | Lswitch(l, sw) ->
      count_default bv sw ;
      count bv l;
      List.iter (fun (_, l) -> count bv l) sw.sw_consts;
      List.iter (fun (_, l) -> count bv l) sw.sw_blocks
    | Lstringswitch(l, sw, d) ->
      count bv l ;
      List.iter (fun (_, l) -> count bv l) sw ;
      begin match d with
        | Some d -> count bv d 
        | None -> ()
      end        
    (* x2 for native backend *)
    (* begin match sw with *)
    (* | []|[_] -> count bv d *)
    (* | _ -> count bv d ; count bv d *)
    (* end *)      
    | Lstaticraise (i,ls) -> List.iter (count bv) ls
    | Lstaticcatch(l1, (i,_), l2) -> count bv l1; count bv l2
    | Ltrywith(l1, v, l2) -> count bv l1; count bv l2
    | Lifthenelse(l1, l2, l3) -> count bv l1; count bv l2; count bv l3
    | Lsequence(l1, l2) -> count bv l1; count bv l2 
    | Lsend(_, m, o, ll, _) -> 
      count bv m ;
      count bv o;
      List.iter (count bv) ll
    | Lifused(v, l) ->
      if used v then count bv l
  and count_default bv sw = 
    match sw.sw_failaction with
    | None -> ()
    | Some al ->
      let nconsts = List.length sw.sw_consts
      and nblocks = List.length sw.sw_blocks in
      if nconsts < sw.sw_numconsts && nblocks < sw.sw_numblocks
      then 
        begin (* default action will occur twice in native code *)
          count bv al ; count bv al
        end 
      else 
        begin (* default action will occur once *)
          assert (nconsts < sw.sw_numconsts || nblocks < sw.sw_numblocks) ;
          count bv al
        end
  in
  count Ident_map.empty  lam;
  occ

