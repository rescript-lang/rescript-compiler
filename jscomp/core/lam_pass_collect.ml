(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
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








(** Check, it is shared across ident_tbl, 
    Only [Lassign] will break such invariant,
    how about guarantee that [Lassign] only check the local ref 
    and we track which ids are [Lassign]ed
*)
(**
   might not be the same due to refinement
   assert (old.arity = v) 
*)
let annotate (meta : Lam_stats.t)  rec_flag  (k:Ident.t) (arity : Lam_arity.t) lambda = 
  match Ident_hashtbl.find_opt  meta.ident_tbl k  with 
  | None -> (** FIXME: need do a sanity check of arity is NA or Determin(_,[],_) *)
    Ident_hashtbl.add meta.ident_tbl k 
      (FunctionId {arity; lambda; rec_flag})
  |  Some (FunctionId old)  ->  

    old.arity <- arity  (* due to we keep refining arity analysis after each round*)      
  | _ -> assert false (* TODO -- avoid exception *)


(** it only make senses recording arities for 
    function definition,
    alias propgation - and toplevel identifiers, this needs to be exported
*)
let collect_helper  (meta : Lam_stats.t) (lam : Lam.t)  = 
  let rec collect_bind rec_flag
      (kind : Lam.let_kind) 
      (ident : Ident.t)
      (lam : Lam.t) = 
    match lam with 
    | Lconst v 
      -> 
      Ident_hashtbl.replace meta.ident_tbl ident (Constant v); (** *)
    | Lprim {primitive = Pmakeblock (_, _, Immutable ) ; args=  ls}
      -> 
      Ident_hashtbl.replace meta.ident_tbl ident 
        (Lam_util.kind_of_lambda_block Normal ls);
      List.iter collect ls     
    | Lprim {primitive = Pnull_to_opt; 
             args = ([ Lvar _] as ls) ; _}
      ->
      Ident_hashtbl.replace meta.ident_tbl ident 
        (Lam_util.kind_of_lambda_block Null ls )    
    | Lprim {primitive = Pundefined_to_opt; 
             args = ([ Lvar _] as ls); _}
      ->
      Ident_hashtbl.replace meta.ident_tbl ident 
        (Lam_util.kind_of_lambda_block Undefined ls )
    | Lprim {primitive = Pnull_undefined_to_opt;
             args = ([ Lvar _] as ls);}
      ->
      Ident_hashtbl.replace meta.ident_tbl ident 
        (Lam_util.kind_of_lambda_block Null_undefined ls )
    | Lglobal_module v  
      -> 
      Lam_util.alias_ident_or_global meta  ident v (Module  v) kind; 
    | Lvar v 
      -> 
      (
        (* if Ident.global v then  *)
        Lam_util.alias_ident_or_global meta  ident v NA kind
        (* enven for not subsitution, it still propogate some properties *)
        (* else () *)
      )
    | Lfunction{ params; body}
      (** TODO record parameters ident ?, but it will be broken after inlining *)  
      -> 
      (** TODO could be optimized in one pass? 
          -- since collect would iter everywhere,
          so -- it would still iterate internally
      *)

      List.iter (fun p -> Ident_hashtbl.add meta.ident_tbl p Parameter ) params;
      let arity = Lam_arity_analysis.get_arity meta lam in       
      annotate meta rec_flag ident  arity lam; 
      collect body
    | x -> 
      collect x ;
      if Ident_set.mem ident meta.export_idents then 
        annotate meta rec_flag ident (Lam_arity_analysis.get_arity meta x ) lam


  and collect  (lam : Lam.t)  =
    match lam with 

    (** TODO: 
        how about module aliases..
        record dependency
        --- tricky -- if we inlining, 
        is it safe to remove it? probably not...
    *)
    | Lconst _ -> ()
    | Lvar _ -> ()
    | Lapply{fn = l1; args =  ll; _} ->
      collect  l1; List.iter collect  ll
    | Lfunction { params; body =  l} -> (* functor ? *)
      List.iter (fun p -> Ident_hashtbl.add meta.ident_tbl p Parameter ) params;
      collect  l
    | Llet (kind,ident,arg,body) -> 
      collect_bind Non_rec kind ident arg ; collect body
    | Lletrec (bindings, body) -> 
      List.iter (fun (ident,arg) -> collect_bind Rec  Strict ident arg ) bindings;
      collect body
    | Lglobal_module _ -> ()
    | Lprim {args; _} -> List.iter collect  args
    | Lswitch(l, {sw_failaction; sw_consts; sw_blocks}) ->
      collect  l;
      List.iter (fun (_, l) -> collect  l) sw_consts;
      List.iter (fun (_, l) -> collect  l) sw_blocks;
      begin match sw_failaction with 
        | None -> ()
        | Some x -> collect x
      end
    | Lstringswitch(l, sw, d) ->
      collect  l ;
      List.iter (fun (_, l) -> collect  l) sw ;
      begin match d with
        | Some d -> collect d 
        | None -> ()
      end
    | Lstaticraise (code,ls) -> 
      List.iter collect  ls
    | Lstaticcatch(l1, (_,_), l2) -> collect  l1; collect  l2
    | Ltrywith(l1, _, l2) -> collect  l1; collect  l2
    | Lifthenelse(l1, l2, l3) -> collect  l1; collect  l2; collect  l3
    | Lsequence(l1, l2) -> collect  l1; collect  l2
    | Lwhile(l1, l2) -> collect  l1; collect l2
    | Lfor(_, l1, l2, dir, l3) -> collect  l1; collect  l2; collect  l3
    | Lassign(v, l) ->
      (* Lalias-bound variables are never assigned, so don't increase
         v's refcollect *)
      collect  l
    | Lsend(_, m, o, ll, _) -> List.iter collect  (m::o::ll)
    | Lifused(_, l) -> collect  l in collect lam 



let count_alias_globals 
    env 
    filename
    export_idents
    export_sets 
    (lam : Lam.t) : Lam_stats.t =
  let meta : Lam_stats.t = 
    {alias_tbl = Ident_hashtbl.create 31 ; 
     ident_tbl = Ident_hashtbl.create 31;

     exports =  export_idents;
     filename;
     env;
     export_idents = export_sets;
    } in 
  collect_helper  meta lam ; 
  meta
