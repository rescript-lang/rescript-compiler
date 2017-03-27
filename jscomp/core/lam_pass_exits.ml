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
(* Adapted for Javascript backend: Hongbo Zhang                        *)

(*
   TODO: 
   we should have a pass called, always inlinable
   as long as its length is smaller than [exit=exit_id], for example

   {[
     switch(box_name)
       {case "":exit=178;break;
        case "b":exit=178;break;
        case "h":box_type=/* Pp_hbox */0;break;
        case "hov":box_type=/* Pp_hovbox */3;break;
        case "hv":box_type=/* Pp_hvbox */2;break;
        case "v":box_type=/* Pp_vbox */1;break;
        default:box_type=invalid_box(/* () */0);}

       switch(exit){case 178:box_type=/* Pp_box */4;break}
   ]}
*)

(* Count occurrences of (exit n ...) statements *)
let count_exit exits i =
  match 
    (Int_hashtbl.find_opt exits i)
  with
  | None -> 0
  | Some v -> !v 

and incr_exit exits i =
  Int_hashtbl.modify_or_init exits i incr (fun _ -> ref 1)


let count_helper  (lam : Lam.t) : int ref Int_hashtbl.t  = 
  let exits  = Int_hashtbl.create 17 in
  let rec count (lam : Lam.t) = 
    match lam with 
    | Lstaticraise (i,ls) -> incr_exit exits i ; List.iter count ls
    | Lstaticcatch (l1,(i,[]),Lstaticraise (j,[])) ->
      (* i will be replaced by j in l1, so each occurence of i in l1
         increases j's ref count *)
      count l1 ;
      let ic = count_exit exits i in
      Int_hashtbl.modify_or_init exits j (fun x -> x := !x + ic) (fun _ -> ref ic)
    | Lstaticcatch(l1, (i,_), l2) ->
      count l1;
      (* If l1 does not contain (exit i),
         l2 will be removed, so don't count its exits *)
      if count_exit exits i > 0 
      then
        count l2
    | Lstringswitch(l, sw, d) ->
      count l;
      List.iter (fun (_, l) -> count l) sw;
      begin 
        match  d with
        | None -> ()
        | Some d -> 
          (* See https://github.com/ocaml/ocaml/commit/fcf3571123e2c914768e34f1bd17e4cbaaa7d212#diff-704f66c0fa0fc9339230b39ce7d90919 
             might only necessary for native backend
          *)
          count d
          (* begin match sw with *)
          (* | []|[_] -> count d *)
          (* | _ -> count d; count d (\** ASK: default will get replicated *\) *)
          (* end *)
      end
    | Lvar _| Lconst _ -> ()
    | Lapply{fn = l1; args =  ll; _} -> count l1; List.iter count ll
    | Lfunction {body = l} -> count l
    | Llet(_, _, l1, l2) ->
      count l2; count l1
    | Lletrec(bindings, body) ->
      List.iter (fun (_, l) -> count l) bindings;
      count body
    | Lglobal_module _ -> ()
    | Lprim {args;  _} -> List.iter count args
    | Lswitch(l, sw) ->
      count_default sw ;
      count l;
      List.iter (fun (_, l) -> count l) sw.sw_consts;
      List.iter (fun (_, l) -> count l) sw.sw_blocks
    | Ltrywith(l1, v, l2) -> count l1; count l2
    | Lifthenelse(l1, l2, l3) -> count l1; count l2; count l3
    | Lsequence(l1, l2) -> count l1; count l2
    | Lwhile(l1, l2) -> count l1; count l2
    | Lfor(_, l1, l2, dir, l3) -> count l1; count l2; count l3
    | Lassign(_, l) -> count l
    | Lsend(_, m, o, ll, _) -> count m; count o; List.iter count ll
    | Lifused(_, l) -> count l 

  and count_default sw =
    match sw.sw_failaction with
    | None -> ()
    | Some al ->
      let nconsts = List.length sw.sw_consts
      and nblocks = List.length sw.sw_blocks in
      if
        nconsts < sw.sw_numconsts && nblocks < sw.sw_numblocks
      then 
        (*
              Reason: for pattern match, 
              we will  test whether it is 
              an integer or block, both have default cases
              predicate: [sw_numconsts] vs nconsts
          *)

        begin 
          count al ; count al
        end 
      else 
        begin (* default action will occur once *)
          assert (nconsts < sw.sw_numconsts || nblocks < sw.sw_numblocks) ;
          count al
        end in 
  count lam ; 
  exits
;;

type subst_tbl = (Ident.t list * Lam.t) Int_hashtbl.t

(*
   Second pass simplify  ``catch body with (i ...) handler''
      - if (exit i ...) does not occur in body, suppress catch
      - if (exit i ...) occurs exactly once in body,
        substitute it with handler
      - If handler is a single variable, replace (exit i ..) with it
*)
(*
  Note:
    In ``catch body with (i x1 .. xn) handler''
     Substituted expression is
      let y1 = x1 and ... yn = xn in
      handler[x1 <- y1 ; ... ; xn <- yn]
     For the sake of preserving the uniqueness  of bound variables.
   ASKS: This documentation seems outdated
     (No alpha conversion of ``handler'' is presently needed, since
     substitution of several ``(exit i ...)''
     occurs only when ``handler'' is a variable.)
*)


let subst_helper (subst : subst_tbl) (query : int -> int) lam = 
  let rec simplif (lam : Lam.t) = 
    match lam with 
    | Lstaticraise (i,[])  ->
      begin match Int_hashtbl.find_opt subst i with
        | Some (_, handler) -> handler
        | None -> lam
      end
    | Lstaticraise (i,ls) ->
      let ls = List.map simplif ls in
      begin 
        match Int_hashtbl.find_opt subst i with
        | Some (xs,handler) -> 
          let ys = List.map Ident.rename xs in
          let env =
            List.fold_right2
              (fun x y t -> Ident_map.add x (Lam.var y) t)
              xs ys Ident_map.empty in
          List.fold_right2
            (fun y l r -> Lam.let_ Alias y l r)
            ys ls 
            (Lam_util.subst_lambda  env  handler)
        | None -> Lam.staticraise i ls
      end
    | Lstaticcatch (l1,(i,[]),(Lstaticraise (j,[]) as l2)) ->
      Int_hashtbl.add subst i ([],simplif l2) ;
      simplif l1 (** l1 will inline the exit handler *)
    | Lstaticcatch (l1,(i,xs),l2) ->
      begin 
        match query i, l2 with
        | 0,_ -> simplif l1

        (* Note that 
           for [query] result = 2, 
           the non-inline cost is 
           {[
             var exit ;

             exit = 11;
             exit = 11;

             switch(exit){
               case exit = 11 : body ; break
             }

           ]}
           the inline cost is 

           {[
             body;
             body;
           ]}

           when [i] is negative, we can not inline in general, 
           since the outer is a traditional [try .. catch] body, 
           if it is guaranteed to be non throw, then we can inline
        *)
        | ( _ , Lvar _
          | _, Lconst _) ->  
          Int_hashtbl.add subst i (xs,simplif l2) ;
          simplif l1 (** l1 will inline *)
        | 1,_ when i >= 0 -> (** Ask: Note that we have predicate i >=0 *)
          Int_hashtbl.add subst i (xs,simplif l2) ;
          simplif l1 (** l1 will inline *)
        | j,_ ->

          (** TODO: better heuristics, also if we can group same exit code [j] 
              in a very early stage -- maybe we can define our enhanced [Lambda] 
              representation and counter can be more precise, for example [apply] 
              does not need patch from the compiler

              FIXME:   when inlining, need refresh local bound identifiers
          *)

          let ok_to_inline = 
            i >=0 && 
            (Lam.no_bounded_variables l2) &&
            (let lam_size = Lam_analysis.size l2 in
             (j <= 2 && lam_size < Lam_analysis.exit_inline_size   )
             || lam_size < 5)
            (*TODO: when we do the case merging on the js side, 
              the j is not very indicative                
            *)             
          in 
          if ok_to_inline 
             (* #1438 when the action containes bounded variable 
                to keep the invariant, everytime, we do an inlining,
                we need refresh, just refreshing once is not enough
             *)
          then 
            begin 
              Ext_log.dwarn __LOC__ "FUCK%d@." i;
              Int_hashtbl.add subst i (xs, Lam_bounded_vars.refresh @@ simplif l2) ;
              simplif l1 (** l1 will inline *)
            end
          else Lam.staticcatch (simplif l1) (i,xs) (simplif l2)
      end

    | Lvar _|Lconst _  -> lam
    | Lapply {fn = l1; args =  ll;  loc; status } -> 
      Lam.apply (simplif l1) (List.map simplif ll) loc status
    | Lfunction {arity; function_kind; params; body =  l} -> 
      Lam.function_ ~arity ~function_kind ~params ~body:(simplif l)
    | Llet (kind, v, l1, l2) -> 
      Lam.let_ kind v (simplif l1) (simplif l2)
    | Lletrec (bindings, body) ->
      Lam.letrec
        ( List.map (fun (v, l) -> (v, simplif l)) bindings) 
        (simplif body)
    | Lglobal_module _ -> lam 
    | Lprim {primitive; args; loc} -> 
      let args = List.map simplif args in
      Lam.prim ~primitive ~args loc
    | Lswitch(l, sw) ->
      let new_l = simplif l
      and new_consts =  List.map (fun (n, e) -> (n, simplif e)) sw.sw_consts
      and new_blocks =  List.map (fun (n, e) -> (n, simplif e)) sw.sw_blocks
      and new_fail = 
        begin match sw.sw_failaction with 
          | None   -> None
          | Some x -> Some (simplif x) end in
      Lam.switch
        new_l
        { 
          sw with 
          sw_consts = new_consts ;
          sw_blocks = new_blocks; 
          sw_failaction = new_fail}
    | Lstringswitch(l,sw,d) ->
      Lam.stringswitch
        (simplif l) (List.map (fun (s,l) -> s,simplif l) sw)
        (begin match d with None -> None | Some d -> Some (simplif d) end)
    | Ltrywith (l1, v, l2) -> 
      Lam.try_ (simplif l1) v (simplif l2)
    | Lifthenelse (l1, l2, l3) -> 
      Lam.if_ (simplif l1) (simplif l2) (simplif l3)
    | Lsequence (l1, l2) -> Lam.seq (simplif l1) (simplif l2)
    | Lwhile (l1, l2) -> Lam.while_ (simplif l1) (simplif l2)
    | Lfor (v, l1, l2, dir, l3) ->
      Lam.for_ v (simplif l1) (simplif l2) dir (simplif l3)
    | Lassign (v, l) -> 
      Lam.assign v (simplif l)
    | Lsend (k, m, o, ll, loc) ->
      Lam.send k (simplif m) (simplif o) (List.map simplif ll) loc
    | Lifused (v, l) -> 
      Lam.ifused v (simplif l)
  in 
  simplif lam 

let simplify_exits (lam : Lam.t) =
  let exits = count_helper lam in
  subst_helper (Int_hashtbl.create 17 ) (count_exit exits) lam

(* Compile-time beta-reduction of functions immediately applied:
      Lapply(Lfunction(Curried, params, body), args, loc) ->
        let paramN = argN in ... let param1 = arg1 in body
      Lapply(Lfunction(Tupled, params, body), [Lprim(Pmakeblock(args))], loc) ->
        let paramN = argN in ... let param1 = arg1 in body
   Assumes |args| = |params|.
*)
