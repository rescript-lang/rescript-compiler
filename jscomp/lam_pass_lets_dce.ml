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


open Asttypes

exception Real_reference

let rec eliminate_ref id (lam : Lambda.lambda) = 
  match lam with  (** we can do better escape analysis in Javascript backend *)
  | Lvar v ->
    if Ident.same v id then raise Real_reference else lam
  | Lprim(Pfield (0,_), [Lvar v]) when Ident.same v id ->
    Lvar id
  | Lfunction(kind, params, body) as lam ->
    if Lambda.IdentSet.mem id (Lambda.free_variables lam)
    then raise Real_reference
    else lam
  (* In Javascript backend, its okay, we can reify it later
     a failed case 
     {[
       for i = .. 
           let v = ref 0 
               for j = .. 
                   incr v 
                     a[j] = ()=>{!v}

     ]}
     here v is captured by a block, and it's a loop mutable value,
     we have to generate 
     {[
       for i = .. 
           let v = ref 0 
               (function (v){for j = .. 
                                   a[j] = ()=>{!v}}(v)

     ]}
     now, v is a real reference 
     TODO: we can refine analysis in later
  *)
  (* Lfunction(kind, params, eliminate_ref id body) *)
  | Lprim(Psetfield(0, _,_), [Lvar v; e]) when Ident.same v id ->
    Lassign(id, eliminate_ref id e)
  | Lprim(Poffsetref delta, [Lvar v]) when Ident.same v id ->
    Lassign(id, Lprim(Poffsetint delta, [Lvar id]))
  | Lconst _  -> lam
  | Lapply(e1, el, loc) ->
    Lapply(eliminate_ref id e1, List.map (eliminate_ref id) el, loc)
  | Llet(str, v, e1, e2) ->
    Llet(str, v, eliminate_ref id e1, eliminate_ref id e2)
  | Lletrec(idel, e2) ->
    Lletrec(List.map (fun (v, e) -> (v, eliminate_ref id e)) idel,
            eliminate_ref id e2)
  | Lprim(p, el) ->
    Lprim(p, List.map (eliminate_ref id) el)
  | Lswitch(e, sw) ->
    Lswitch(eliminate_ref id e,
            {sw_numconsts = sw.sw_numconsts;
             sw_consts =
               List.map (fun (n, e) -> (n, eliminate_ref id e)) sw.sw_consts;
             sw_numblocks = sw.sw_numblocks;
             sw_blocks =
               List.map (fun (n, e) -> (n, eliminate_ref id e)) sw.sw_blocks;
             sw_failaction =
               Misc.may_map (eliminate_ref id) sw.sw_failaction; })
  | Lstringswitch(e, sw, default) ->
    Lstringswitch
      (eliminate_ref id e,
       List.map (fun (s, e) -> (s, eliminate_ref id e)) sw,
       Misc.may_map (eliminate_ref id) default)
  | Lstaticraise (i,args) ->
    Lstaticraise (i,List.map (eliminate_ref id) args)
  | Lstaticcatch(e1, i, e2) ->
    Lstaticcatch(eliminate_ref id e1, i, eliminate_ref id e2)
  | Ltrywith(e1, v, e2) ->
    Ltrywith(eliminate_ref id e1, v, eliminate_ref id e2)
  | Lifthenelse(e1, e2, e3) ->
    Lifthenelse(eliminate_ref id e1,
                eliminate_ref id e2,
                eliminate_ref id e3)
  | Lsequence(e1, e2) ->
    Lsequence(eliminate_ref id e1, eliminate_ref id e2)
  | Lwhile(e1, e2) ->
    Lwhile(eliminate_ref id e1, eliminate_ref id e2)
  | Lfor(v, e1, e2, dir, e3) ->
    Lfor(v, eliminate_ref id e1, eliminate_ref id e2,
         dir, eliminate_ref id e3)
  | Lassign(v, e) ->
    Lassign(v, eliminate_ref id e)
  | Lsend(k, m, o, el, loc) ->
    Lsend(k, eliminate_ref id m, eliminate_ref id o,
          List.map (eliminate_ref id) el, loc)
  | Levent(l, ev) ->
    Levent(eliminate_ref id l, ev)
  | Lifused(v, e) ->
    Lifused(v, eliminate_ref id e)

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

type occ_tbl  = (Ident.t, used_info) Hashtbl.t
(* First pass: count the occurrences of all let-bound identifiers *)

type local_tbl = used_info  Ident_map.t

let dummy_info () = {times =  0 ; captured = false }
(* y is untouched *)

let absorb_info (x : used_info) (y : used_info) = 
  match x, y with
  | {times = x0} , {times = y0; captured } -> 
    x.times <- x0 + y0;
    if captured then x.captured <- true

let lets_helper (count_var : Ident.t -> used_info) lam = 
  let subst = Hashtbl.create 31 in
  let used v = (count_var v ).times > 0 in
  let rec simplif (lam : Lambda.lambda) = 
    match lam with 
    | Lvar v  ->
      begin try Hashtbl.find subst v with Not_found -> lam end
    | Llet( (Strict | Alias | StrictOpt) , v, Lvar w, l2) 
      ->
      Hashtbl.add subst v (simplif (Lvar w));
      simplif l2
    | Llet((Strict | StrictOpt as kind) ,
           v, (Lprim((Pmakeblock(0, tag_info, Mutable) as prim), [linit])), lbody)
      ->
      let slinit = simplif linit in
      let slbody = simplif lbody in
      begin 
        try (** TODO: record all references variables *)
          Lam_util.refine_let
            ~kind:Variable v slinit (eliminate_ref v slbody)
        with Real_reference ->
          Lam_util.refine_let 
            ~kind v (Lprim(prim, [slinit]))
            slbody
      end
    | Llet(Alias, v, l1, l2) ->
      (** For alias, [l1] is pure, we can always inline,
          when captured, we should avoid recomputation
      *)
      begin 
        match count_var v, l1  with
        | {times = 0; _}, _  -> simplif l2 
        | {times = 1; captured = false }, _ 
        | {times = 1; captured = true }, (Lconst _ | Lvar _)
          ->
          Hashtbl.add subst v (simplif l1); simplif l2
        | _ -> Llet(Alias, v, simplif l1, simplif l2)
      end
    | Llet(StrictOpt as kind, v, l1, l2) ->
      (** can not be inlined since [l1] depend on the store
          {[
            let v = [|1;2;3|]
          ]}
          get [StrictOpt] here,  we can not inline v, 
          since the value of [v] can be changed
      *)
      if not @@ used v 
      then simplif l2
      else Lam_util.refine_let ~kind v (simplif l1 ) (simplif l2)
    (* TODO: check if it is correct rollback to [StrictOpt]? *)

    | Llet((Strict | Variable as kind), v, l1, l2) -> 
      if not @@ used v 
      then
        let l1 = simplif l1 in
        let l2 = simplif l2 in
        if Lam_analysis.no_side_effects l1 
        then l2 
        else Lsequence(l1, l2)
      else Lam_util.refine_let ~kind v (simplif l1) (simplif l2)

    | Lifused(v, l) ->
      if used  v then
        simplif l
      else Lambda.lambda_unit
    | Lsequence(Lifused(v, l1), l2) ->
      if used v 
      then Lsequence(simplif l1, simplif l2)
      else simplif l2
    | Lsequence(l1, l2) -> Lsequence(simplif l1, simplif l2)

    | Lapply(Lfunction(Curried, params, body), args, _)
      when  Ext_list.same_length params args ->
      simplif (Lam_beta_reduce.beta_reduce  params body args)
    | Lapply(Lfunction(Tupled, params, body), [Lprim(Pmakeblock _, args)], _)
      (** TODO: keep track of this parameter in ocaml trunk,
          can we switch to the tupled backend?
      *)
      when  Ext_list.same_length params  args ->
      simplif (Lam_beta_reduce.beta_reduce params body args)

    | Lapply(l1, ll, loc) -> Lapply(simplif l1, List.map simplif ll, loc)
    | Lfunction(kind, params, l) -> Lfunction(kind, params, simplif l)
    | Lconst _ -> lam
    | Lletrec(bindings, body) ->
      Lletrec(List.map (fun (v, l) -> (v, simplif l)) bindings, simplif body)
    | Lprim(p, ll) -> Lprim(p, List.map simplif ll)
    | Lswitch(l, sw) ->
      let new_l = simplif l
      and new_consts =  List.map (fun (n, e) -> (n, simplif e)) sw.sw_consts
      and new_blocks =  List.map (fun (n, e) -> (n, simplif e)) sw.sw_blocks
      and new_fail = Misc.may_map simplif sw.sw_failaction in
      Lswitch
        (new_l,
         {sw with sw_consts = new_consts ; sw_blocks = new_blocks;
                  sw_failaction = new_fail})
    | Lstringswitch (l,sw,d) ->
      Lstringswitch
        (simplif l,List.map (fun (s,l) -> s,simplif l) sw,
         Misc.may_map simplif d)
    | Lstaticraise (i,ls) ->
      Lstaticraise (i, List.map simplif ls)
    | Lstaticcatch(l1, (i,args), l2) ->
      Lstaticcatch (simplif l1, (i,args), simplif l2)
    | Ltrywith(l1, v, l2) -> Ltrywith(simplif l1, v, simplif l2)
    | Lifthenelse(l1, l2, l3) -> Lifthenelse(simplif l1, simplif l2, simplif l3)
    | Lwhile(l1, l2) -> Lwhile(simplif l1, simplif l2)
    | Lfor(v, l1, l2, dir, l3) ->
      Lfor(v, simplif l1, simplif l2, dir, simplif l3)
    | Lassign(v, l) -> Lassign(v, simplif l)
    | Lsend(k, m, o, ll, loc) ->
      Lsend(k, simplif m, simplif o, List.map simplif ll, loc)
    | Levent(l, ev) -> Levent(simplif l, ev)
  in simplif lam ;;


(* To transform let-bound references into variables *)
let apply_lets  occ lambda = 
  let count_var v =
    try
      Hashtbl.find occ v
    with Not_found -> dummy_info () in
  lets_helper count_var lambda      

let collect_occurs  lam : occ_tbl =
  let occ : occ_tbl = Hashtbl.create 83 in
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

  (* Current use count of a variable. *)
  let used v = 
    match Hashtbl.find occ v with 
    | exception Not_found -> false 
    | {times ; _} -> times > 0  in

  (* Entering a [let].  Returns updated [bv]. *)
  let bind_var bv ident =
    let r = dummy_info () in
    Hashtbl.add occ ident r;
    Ident_map.add ident r bv in

  (* Record a use of a variable *)
  let add_one_use bv ident  =
    match Ident_map.find ident bv with 
    | r  -> r.times <- r.times + 1 
    | exception Not_found ->
      (* ident is not locally bound, therefore this is a use under a lambda
         or within a loop.  Increase use count by 2 -- enough so
         that single-use optimizations will not apply. *)
      match Hashtbl.find occ ident with 
      | r -> absorb_info r {times = 1; captured =  true}
      | exception Not_found ->
        (* Not a let-bound variable, ignore *)
        () in

  let inherit_use bv ident bid =
    let n = try Hashtbl.find occ bid with Not_found -> dummy_info () in
    match Ident_map.find ident bv with 
    | r  -> absorb_info r n
    | exception Not_found ->
      (* ident is not locally bound, therefore this is a use under a lambda
         or within a loop.  Increase use count by 2 -- enough so
         that single-use optimizations will not apply. *)
      match Hashtbl.find occ ident with 
      | r -> absorb_info r {n with captured = true} 
      | exception Not_found ->
        (* Not a let-bound variable, ignore *)
        () in

  let rec count (bv : local_tbl) (lam : Lambda.lambda) = 
    match lam with 
    | Lfunction(kind, params, l) ->
      count Ident_map.empty l
    (** when entering a function local [bv] 
        is cleaned up, so that all closure variables will not be
        carried over, since the parameters are never rebound, 
        so it is fine to kep it empty
    *)
    | Lvar v ->
      add_one_use bv v 
    | Llet(_, v, Lvar w, l2)  ->
      (* v will be replaced by w in l2, so each occurrence of v in l2
         increases w's refcount *)
      count (bind_var bv v) l2;
      inherit_use bv w v 
    (* | Lprim(Pmakeblock _, ll)  *)
    (*     ->  *)
    (*       List.iter (fun x -> count bv x ; count bv x) ll *)
    (* | Llet(kind, v, (Lprim(Pmakeblock _, _) as l1),l2) -> *)
    (*     count (bind_var bv v) l2; *)
    (*     (\* If v is unused, l1 will be removed, so don't count its variables *\) *)
    (*     if kind = Strict || count_var v > 0 then *)
    (*       count bv l1; count bv l1 *)

    | Llet(kind, v, l1, l2) ->
      count (bind_var bv v) l2;
      (* If v is unused, l1 will be removed, so don't count its variables *)
      if kind = Strict || used v then count bv l1

    | Lprim(_, ll) -> List.iter (count bv ) ll

    | Lletrec(bindings, body) ->
      List.iter (fun (v, l) -> count bv l) bindings;
      count bv body
    | Lapply(Lfunction(Curried, params, body), args, _)
      when  Ext_list.same_length params args ->
      count bv (Lam_beta_reduce.beta_reduce  params body args)
    | Lapply(Lfunction(Tupled, params, body), [Lprim(Pmakeblock _, args)], _)
      when  Ext_list.same_length params  args ->
      count bv (Lam_beta_reduce.beta_reduce   params body args)
    | Lapply(l1, ll, _) ->
      count bv l1; List.iter (count bv) ll
    | Lassign(_, l) ->
      (* Lalias-bound variables are never assigned, so don't increase
         this ident's refcount *)
      count bv l
    | Lconst cst -> ()
    | Lswitch(l, sw) ->
      count_default bv sw ;
      count bv l;
      List.iter (fun (_, l) -> count bv l) sw.sw_consts;
      List.iter (fun (_, l) -> count bv l) sw.sw_blocks
    | Lstringswitch(l, sw, d) ->
      count bv l ;
      List.iter (fun (_, l) -> count bv l) sw ;
      begin 
        match d with
        | Some d -> count bv d 
        (* begin match sw with *)
        (* | []|[_] -> count bv d *)
        (* | _ -> count bv d ; count bv d *)
        (* end *)
        | None -> ()
      end
    | Lstaticraise (i,ls) -> List.iter (count bv) ls
    | Lstaticcatch(l1, (i,_), l2) -> count bv l1; count bv l2
    | Ltrywith(l1, v, l2) -> count bv l1; count bv l2
    | Lifthenelse(l1, l2, l3) -> count bv l1; count bv l2; count bv l3
    | Lsequence(l1, l2) -> count bv l1; count bv l2
    | Lwhile(l1, l2) -> count Ident_map.empty l1; count Ident_map.empty l2
    | Lfor(_, l1, l2, dir, l3) -> 
      count bv l1;
      count bv l2; 
      count Ident_map.empty l3
    | Lsend(_, m, o, ll, _) -> List.iter (count bv) (m::o::ll)
    | Levent(l, _) -> count bv l
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

let simplify_lets  (lam : Lambda.lambda) = 
  let occ =  collect_occurs  lam in 
  apply_lets  occ   lam
