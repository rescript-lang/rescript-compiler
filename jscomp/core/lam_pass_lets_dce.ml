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
    | Llet(Strict as kind,
           v, (Lprim {primitive = (Pmakeblock(0, tag_info, Mutable) 
                                   as primitive); 
                      args = [linit] ; loc}), lbody)
      ->
      let slinit = simplif linit in
      let slbody = simplif lbody in
      begin 
        try (** TODO: record all references variables *)
          Lam_util.refine_let
            ~kind:Variable v slinit
             (Lam_pass_eliminate_ref.eliminate_ref v slbody)
        with Lam_pass_eliminate_ref.Real_reference ->
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
                 ((
                     Const_int _ | Const_char _ | Const_float _ | Const_int32 _ 
                     | Const_nativeint _ )
                 | Const_pointer _ |Const_js_true | Const_js_false) (* could be poly-variant [`A] -> [65a]*)
              | Lprim {primitive = Pfield (_);
                       args = [ 
                         Lglobal_module _
                       ]}
              ) 
          (* Const_int64 is no longer primitive
             Note for some constant which is not 
             inlined, we can still record it and
             do constant folding independently              
          *)
          ->
          Ident_hashtbl.add subst v (simplif l1); simplif l2
        | _, Lconst (Const_string s ) -> 
          (** only "" added for later inlining *)
          Ident_hashtbl.add string_table v s;
          Lam.let_ Alias v l1 (simplif l2)
          (* we need move [simplif l2] later, since adding Hashtbl does have side effect *)
        | _ -> Lam.let_ Alias v (simplif l1) (simplif l2)
        (* for Alias, in most cases [l1] is already simplified *)
      end
    | Llet(StrictOpt as kind, v, l1, lbody) ->
      (** can not be inlined since [l1] depend on the store
          {[
            let v = [|1;2;3|]
          ]}
          get [StrictOpt] here,  we can not inline v, 
          since the value of [v] can be changed
          
          GPR #1476 
          Note to pass the sanitizer, we do need remove dead code (not just best effort)
          This logic is tied to {!Lam_pass_count.count}
          {[
            if kind = Strict || used v then count bv l1
          ]}
          If the code which should be removed is not removed, it will hold references 
          to other variables which is already removed.
      *)
      if not @@ used v 
      then simplif lbody (* GPR #1476 *)
      else
        begin match l1 with 
        | (Lprim {primitive = (Pmakeblock(0, tag_info, Mutable) 
                                    as primitive); 
                       args = [linit] ; loc})
          -> 
          let slinit = simplif linit in
          let slbody = simplif lbody in
          begin 
            try (** TODO: record all references variables *)
              Lam_util.refine_let
                ~kind:Variable v slinit
                (Lam_pass_eliminate_ref.eliminate_ref v slbody)
            with Lam_pass_eliminate_ref.Real_reference ->
              Lam_util.refine_let 
                ~kind v (Lam.prim ~primitive ~args:[slinit] loc)
                slbody
          end

        | _ -> 
          let l1 = simplif l1 in         
          begin match l1 with 
            | Lconst(Const_string s) -> 
              Ident_hashtbl.add string_table v s; 
              (* we need move [simplif lbody] later, since adding Hashtbl does have side effect *)
              Lam.let_ Alias v l1 (simplif lbody)
            | _ -> 
              Lam_util.refine_let ~kind v l1 (simplif lbody)
          end  
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
         | Strict, Lconst((Const_string s))
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

    | Lapply{fn = Lfunction{function_kind =  Curried; params; body};  args; _}
      when  Ext_list.same_length params args ->
      simplif (Lam_beta_reduce.beta_reduce  params body args)
    (* | Lapply{ fn = Lfunction{function_kind = Tupled; params; body}; *)
    (*           args = [Lprim {primitive = Pmakeblock _;  args; _}]; _} *)
    (*   (\** TODO: keep track of this parameter in ocaml trunk, *)
    (*       can we switch to the tupled backend? *)
    (*   *\) *)
    (*   when  Ext_list.same_length params  args -> *)
    (*   simplif (Lam_beta_reduce.beta_reduce params body args) *)

    | Lapply{fn = l1;args =  ll; loc; status} -> 
      Lam.apply (simplif l1) (Ext_list.map simplif ll) loc status
    | Lfunction{arity; function_kind; params; body = l} ->
      Lam.function_ ~arity ~function_kind ~params ~body:(simplif l)
    | Lconst _ -> lam
    | Lletrec(bindings, body) ->
      Lam.letrec 
        (Ext_list.map (fun (v, l) -> (v, simplif l)) bindings) 
        (simplif body)
    | Lprim {primitive=Pstringadd; args = [l;r]; loc } -> 
      begin
        let l' = simplif l in 
        let r' = simplif r in
        let opt_l = 
          match l' with 
          | Lconst((Const_string ls)) -> Some ls 
          | Lvar i -> Ident_hashtbl.find_opt string_table i 
          | _ -> None in 
        match opt_l with   
        | None -> Lam.prim ~primitive:Pstringadd ~args:[l';r'] loc 
        | Some l_s -> 
          let opt_r = 
            match r' with 
            | Lconst ( (Const_string rs)) -> Some rs 
            | Lvar i -> Ident_hashtbl.find_opt string_table i 
            | _ -> None in 
            begin match opt_r with 
            | None -> Lam.prim ~primitive:Pstringadd ~args:[l';r'] loc 
            | Some r_s -> 
              Lam.const (Const_string(l_s^r_s))
            end
      end

    | Lprim {primitive = (Pstringrefu|Pstringrefs) as primitive ; 
      args = [l;r] ; loc 
      } ->  (* TODO: introudce new constant *)
      let l' = simplif l in 
      let r' = simplif r in 
      let opt_l =
         match l' with 
         | Lconst (Const_string ls) -> 
            Some ls 
         | Lvar i -> Ident_hashtbl.find_opt string_table i 
         | _ -> None in 
      begin match opt_l with 
      | None -> Lam.prim ~primitive ~args:[l';r'] loc 
      | Some l_s -> 
        match r with 
        |Lconst((Const_int i)) -> 
          if i < String.length l_s && i >=0  then
            Lam.const ((Const_char l_s.[i]))
          else 
            Lam.prim ~primitive ~args:[l';r'] loc 
        | _ -> 
          Lam.prim ~primitive ~args:[l';r'] loc 
      end    
    | Lglobal_module _ -> lam    
    | Lprim {primitive; args; loc} 
      -> Lam.prim ~primitive ~args:(Ext_list.map simplif args) loc
    | Lswitch(l, sw) ->
      let new_l = simplif l
      and new_consts =  Ext_list.map (fun (n, e) -> (n, simplif e)) sw.sw_consts
      and new_blocks =  Ext_list.map (fun (n, e) -> (n, simplif e)) sw.sw_blocks
      and new_fail = 
        match sw.sw_failaction with 
        | None -> None 
        | Some x -> Some (simplif x)
      in
      Lam.switch
        new_l
        {sw with sw_consts = new_consts ; sw_blocks = new_blocks;
                 sw_failaction = new_fail}
    | Lstringswitch (l,sw,d) ->
      Lam.stringswitch
        (simplif l) (Ext_list.map (fun (s,l) -> s,simplif l) sw)
        (match d with None -> None | Some d -> Some (simplif d))
    | Lstaticraise (i,ls) ->
      Lam.staticraise i (Ext_list.map simplif ls)
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
      Lam.send k (simplif m) (simplif o) (Ext_list.map simplif ll) loc
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
#if BS_DEBUG then 
  Ext_log.dwarn "OCCTBL" "@[%a@]@." Lam_pass_count.pp_occ_tbl occ ;
#end
  apply_lets  occ   lam
