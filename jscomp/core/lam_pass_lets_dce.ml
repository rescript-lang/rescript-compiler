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

let rec eliminate_ref id (lam : Lam.t) = 
  match lam with  (** we can do better escape analysis in Javascript backend *)
  | Lvar v ->
    if Ident.same v id then raise_notrace Real_reference else lam
  | Lprim {primitive = Pfield (0,_); args =  [Lvar v]} when Ident.same v id ->
    Lam.var id
  | Lfunction{ kind; params; body} as lam ->
    if Ident_set.mem id (Lam.free_variables  lam)
    then raise_notrace Real_reference
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
  | Lprim {primitive = Psetfield(0, _,_); 
           args =  [Lvar v; e]} when Ident.same v id ->
    Lam.assign id (eliminate_ref id e)
  | Lprim {primitive = Poffsetref delta ; 
           args =  [Lvar v]; loc } when Ident.same v id ->
    Lam.assign id (Lam.prim ~primitive:(Poffsetint delta) ~args:[Lam.var id] loc)
  | Lconst _  -> lam
  | Lapply{fn = e1; args =  el;  loc; status} ->
    Lam.apply 
      (eliminate_ref id e1)
      (List.map (eliminate_ref id) el)
      loc status
  | Llet(str, v, e1, e2) ->
    Lam.let_ str v (eliminate_ref id e1) (eliminate_ref id e2)
  | Lletrec(idel, e2) ->
    Lam.letrec
      (List.map (fun (v, e) -> (v, eliminate_ref id e)) idel)
      (eliminate_ref id e2)
  | Lprim {primitive ; args ; loc} ->
    Lam.prim  ~primitive ~args:(List.map (eliminate_ref id) args) loc
  | Lswitch(e, sw) ->
    Lam.switch(eliminate_ref id e)
      {sw_numconsts = sw.sw_numconsts;
       sw_consts =
         List.map (fun (n, e) -> (n, eliminate_ref id e)) sw.sw_consts;
       sw_numblocks = sw.sw_numblocks;
       sw_blocks =
         List.map (fun (n, e) -> (n, eliminate_ref id e)) sw.sw_blocks;
       sw_failaction =
         Misc.may_map (eliminate_ref id) sw.sw_failaction; }
  | Lstringswitch(e, sw, default) ->
    Lam.stringswitch
      (eliminate_ref id e)
      (List.map (fun (s, e) -> (s, eliminate_ref id e)) sw)
      (Misc.may_map (eliminate_ref id) default)
  | Lstaticraise (i,args) ->
    Lam.staticraise i (List.map (eliminate_ref id) args)
  | Lstaticcatch(e1, i, e2) ->
    Lam.staticcatch (eliminate_ref id e1) i (eliminate_ref id e2)
  | Ltrywith(e1, v, e2) ->
    Lam.try_ (eliminate_ref id e1) v (eliminate_ref id e2)
  | Lifthenelse(e1, e2, e3) ->
    Lam.if_ (eliminate_ref id e1) (eliminate_ref id e2) (eliminate_ref id e3)
  | Lsequence(e1, e2) ->
    Lam.seq (eliminate_ref id e1) (eliminate_ref id e2)
  | Lwhile(e1, e2) ->
    Lam.while_ (eliminate_ref id e1) (eliminate_ref id e2)
  | Lfor(v, e1, e2, dir, e3) ->
    Lam.for_ v
      (eliminate_ref id e1) 
      (eliminate_ref id e2)
      dir
      (eliminate_ref id e3)
  | Lassign(v, e) ->
    Lam.assign v (eliminate_ref id e)
  | Lsend(k, m, o, el, loc) ->
    Lam.send k 
      (eliminate_ref id m) (eliminate_ref id o)
      (List.map (eliminate_ref id) el) loc
  | Lifused(v, e) ->
    Lam.ifused v (eliminate_ref id e)



let lets_helper (count_var : Ident.t -> Lam_pass_count.used_info) lam = 
  let subst : Lam.t Ident_hashtbl.t = Ident_hashtbl.create 32 in
  let string_table : string Ident_hashtbl.t = Ident_hashtbl.create 32 in  
  let used v = (count_var v ).times > 0 in
  let rec simplif (lam : Lam.t) = 
    match lam with 
    | Lvar v  -> Ident_hashtbl.find_default subst v lam 
    | Llet( (Strict | Alias | StrictOpt) , v, Lvar w, l2) 
      ->
      Ident_hashtbl.add subst v (simplif (Lam.var w));
      simplif l2
    | Llet((Strict | StrictOpt as kind) ,
           v, (Lprim {primitive = (Pmakeblock(0, tag_info, Mutable) 
                                   as primitive); 
                      args = [linit] ; loc}), lbody)
      ->
      let slinit = simplif linit in
      let slbody = simplif lbody in
      begin 
        try (** TODO: record all references variables *)
          Lam_util.refine_let
            ~kind:Variable v slinit (eliminate_ref v slbody)
        with Real_reference ->
          Lam_util.refine_let 
            ~kind v (Lam.prim ~primitive ~args:[slinit] loc)
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
        |  _, (Lconst 
                 (Const_base (
                     Const_int _ | Const_char _ | Const_float _ | Const_int32 _ 
                     | Const_nativeint _ )
                 | Const_pointer _ ) (* could be poly-variant [`A] -> [65a]*)
              | Lprim {primitive = Pfield (_);
                       args = [Lprim {primitive = Pgetglobal _;  _}]}
              ) 
          (* Const_int64 is no longer primitive
             Note for some constant which is not 
             inlined, we can still record it and
             do constant folding independently              
          *)
          ->
          Ident_hashtbl.add subst v (simplif l1); simplif l2
        | _, Lconst (Const_base (Const_string (s,_)) ) -> 
          Ident_hashtbl.add string_table v s;
          Lam.let_ Alias v l1 (simplif l2)
          (* we need move [simplif l2] later, since adding Hashtbl does have side effect *)
        | _ -> Lam.let_ Alias v (simplif l1) (simplif l2)
        (* for Alias, in most cases [l1] is already simplified *)
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
      else 
        let l1 = simplif l1 in         
        begin match l1 with 
        | Lconst(Const_base(Const_string(s,_))) -> 
          Ident_hashtbl.add string_table v s; 
          (* we need move [simplif l2] later, since adding Hashtbl does have side effect *)
          Lam.let_ Alias v l1 (simplif l2)
        | _ -> 
          Lam_util.refine_let ~kind v l1 (simplif l2)
        end  
    (* TODO: check if it is correct rollback to [StrictOpt]? *)

    | Llet((Strict | Variable as kind), v, l1, l2) -> 
      if not @@ used v 
      then
        let l1 = simplif l1 in
        let l2 = simplif l2 in
        if Lam_analysis.no_side_effects l1 
        then l2 
        else Lam.seq l1 l2
      else 
        let l1 = (simplif l1) in 
        
         begin match kind, l1 with 
         | Strict, Lconst(Const_base(Const_string(s,_)))
           -> 
            Ident_hashtbl.add string_table v s;
            Lam.let_ Alias v l1 (simplif l2)
         | _ -> 
           Lam_util.refine_let ~kind v l1 (simplif l2)
        end
    | Lifused(v, l) ->
      if used  v then
        simplif l
      else Lam.unit
    | Lsequence(Lifused(v, l1), l2) ->
      if used v 
      then Lam.seq (simplif l1) (simplif l2)
      else simplif l2
    | Lsequence(l1, l2) -> Lam.seq (simplif l1) (simplif l2)

    | Lapply{fn = Lfunction{kind =  Curried; params; body};  args; _}
      when  Ext_list.same_length params args ->
      simplif (Lam_beta_reduce.beta_reduce  params body args)
    | Lapply{ fn = Lfunction{kind = Tupled; params; body};
              args = [Lprim {primitive = Pmakeblock _;  args; _}]; _}
      (** TODO: keep track of this parameter in ocaml trunk,
          can we switch to the tupled backend?
      *)
      when  Ext_list.same_length params  args ->
      simplif (Lam_beta_reduce.beta_reduce params body args)

    | Lapply{fn = l1;args =  ll; loc; status} -> 
      Lam.apply (simplif l1) (List.map simplif ll) loc status
    | Lfunction{arity; kind; params; body = l} ->
      Lam.function_ ~arity ~kind ~params ~body:(simplif l)
    | Lconst _ -> lam
    | Lletrec(bindings, body) ->
      Lam.letrec 
        (List.map (fun (v, l) -> (v, simplif l)) bindings) 
        (simplif body)
    | Lprim {primitive=Pstringadd; args = [l;r]; loc } -> 
      begin
        let l' = simplif l in 
        let r' = simplif r in
        let opt_l = 
          match l' with 
          | Lconst(Const_base(Const_string(ls,_))) -> Some ls 
          | Lvar i -> Ident_hashtbl.find_opt string_table i 
          | _ -> None in 
        match opt_l with   
        | None -> Lam.prim ~primitive:Pstringadd ~args:[l';r'] loc 
        | Some l_s -> 
          let opt_r = 
            match r' with 
            | Lconst (Const_base (Const_string(rs,_))) -> Some rs 
            | Lvar i -> Ident_hashtbl.find_opt string_table i 
            | _ -> None in 
            begin match opt_r with 
            | None -> Lam.prim ~primitive:Pstringadd ~args:[l';r'] loc 
            | Some r_s -> 
              Lam.const ((Const_base(Const_string(l_s^r_s, None))))
            end
      end

    | Lprim {primitive = (Pstringrefu|Pstringrefs) as primitive ; 
      args = [l;r] ; loc 
      } ->  (* TODO: introudce new constant *)
      let l' = simplif l in 
      let r' = simplif r in 
      let opt_l =
         match l' with 
         | Lconst (Const_base(Const_string(ls,_))) -> 
            Some ls 
         | Lvar i -> Ident_hashtbl.find_opt string_table i 
         | _ -> None in 
      begin match opt_l with 
      | None -> Lam.prim ~primitive ~args:[l';r'] loc 
      | Some l_s -> 
        match r with 
        |Lconst(Const_base(Const_int i)) -> 
          if i < String.length l_s && i >=0  then
            Lam.const (Const_base (Const_char l_s.[i]))
          else 
            Lam.prim ~primitive ~args:[l';r'] loc 
        | _ -> 
          Lam.prim ~primitive ~args:[l';r'] loc 
      end    
    | Lprim {primitive; args; loc} 
      -> Lam.prim ~primitive ~args:(List.map simplif args) loc
    | Lswitch(l, sw) ->
      let new_l = simplif l
      and new_consts =  List.map (fun (n, e) -> (n, simplif e)) sw.sw_consts
      and new_blocks =  List.map (fun (n, e) -> (n, simplif e)) sw.sw_blocks
      and new_fail = Misc.may_map simplif sw.sw_failaction in
      Lam.switch
        new_l
        {sw with sw_consts = new_consts ; sw_blocks = new_blocks;
                 sw_failaction = new_fail}
    | Lstringswitch (l,sw,d) ->
      Lam.stringswitch
        (simplif l) (List.map (fun (s,l) -> s,simplif l) sw)
        (Misc.may_map simplif d)
    | Lstaticraise (i,ls) ->
      Lam.staticraise i (List.map simplif ls)
    | Lstaticcatch(l1, (i,args), l2) ->
      Lam.staticcatch (simplif l1) (i,args) (simplif l2)
    | Ltrywith(l1, v, l2) -> Lam.try_ (simplif l1) v (simplif l2)
    | Lifthenelse(l1, l2, l3) -> 
      Lam.if_ (simplif l1) (simplif l2) (simplif l3)
    | Lwhile(l1, l2) 
      -> 
      Lam.while_ (simplif l1) (simplif l2)
    | Lfor(v, l1, l2, dir, l3) ->
      Lam.for_ v (simplif l1) (simplif l2) dir (simplif l3)
    | Lassign(v, l) -> Lam.assign v (simplif l)
    | Lsend(k, m, o, ll, loc) ->
      Lam.send k (simplif m) (simplif o) (List.map simplif ll) loc
  in simplif lam ;;


(* To transform let-bound references into variables *)
let apply_lets  occ lambda = 
  let count_var v =
    match
      Ident_hashtbl.find_opt occ v 
    with
    | None -> Lam_pass_count.dummy_info ()
    | Some  v -> v in
  lets_helper count_var lambda      

let simplify_lets  (lam : Lam.t) = 
  let occ =  Lam_pass_count.collect_occurs  lam in 
  apply_lets  occ   lam
