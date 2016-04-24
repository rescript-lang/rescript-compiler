(* BuckleScript compiler
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




let annotate (meta : Lam_stats.meta)
    rec_flag    
    (k:Ident.t) (v : Lam_stats.function_arities) lambda = 
  (* Ext_log.dwarn  __LOC__ "%s/%d" k.name k.stamp;     *)
  match Hashtbl.find  meta.ident_tbl k  with 
  | exception Not_found -> 
      Hashtbl.add meta.ident_tbl k (Function {kind = NA; arity = v; lambda; rec_flag})
  |  Function old  ->  
      (** Check, it is shared across ident_tbl, 
          Only [Lassign] will break such invariant,
          how about guarantee that [Lassign] only check the local ref 
          and we track which ids are [Lassign]ed
       *)
      (**
         might not be the same due to refinement
         assert (old.arity = v) 
       *)
      old.arity <- v
      

  | _ -> assert false (* TODO -- avoid exception *)


(** it only make senses recording arities for 
    function definition,
    alias propgation - and toplevel identifiers, this needs to be exported
 *)
let collect_helper  (meta : Lam_stats.meta) (lam : Lambda.lambda)  = 
  let rec collect_bind rec_flag
      (kind : Lambda.let_kind) 
      (ident : Ident.t)
      (lam : Lambda.lambda) = 
    match lam with 
    | Lconst v 
      -> 
      Hashtbl.replace meta.ident_tbl ident (Constant v); (** *)
    | Lprim (Pmakeblock (_, _, Immutable ) , ls)
      -> 
      Hashtbl.replace meta.ident_tbl ident 
        (Lam_util.kind_of_lambda_block Normal ls);
      List.iter collect ls 

    | Lprim (Pccall {prim_name = "js_from_nullable"; _}, ([ Lvar _] as ls))
      ->
      Hashtbl.replace meta.ident_tbl ident 
        (Lam_util.kind_of_lambda_block Null ls )
    | Lprim (Pccall {prim_name = "js_from_def"; _}, ([ Lvar _] as ls))
      ->
      Hashtbl.replace meta.ident_tbl ident 
        (Lam_util.kind_of_lambda_block Undefined ls )
    | Lprim (Pccall {prim_name = "js_from_nullable_def"; _}, ([ Lvar _] as ls))
      ->
      Hashtbl.replace meta.ident_tbl ident 
        (Lam_util.kind_of_lambda_block Null_undefined ls )
      
    | Lprim (Pgetglobal v,[]) 
      -> 
      begin 
        Lam_util.alias meta  ident v (Module  v) kind; 
        begin match kind with 
          | Alias -> ()
          | Strict | StrictOpt | Variable -> 
            Lam_util.add_required_module v meta
        end;
      end
    | Lvar v 
      -> 
        (
         (* if Ident.global v then  *)
         Lam_util.alias meta  ident v NA kind
           (* enven for not subsitution, it still propogate some properties *)
           (* else () *)
        )
    | Lfunction(_, params,l)
        (** TODO record parameters ident ?, but it will be broken after inlining *)  
      -> 
        (** TODO could be optimized in one pass? 
            -- since collect would iter everywhere,
            so -- it would still iterate internally
         *)

      List.iter (fun p -> Hashtbl.add meta.ident_tbl p Parameter ) params;
      let arity = Lam_stats_util.get_arity meta lam in       
      (* Ext_log.dwarn __LOC__ "%s/%d : %a : %a function collected"  *)
      (*   ident.name ident.stamp  *)
      (*   Printlambda.lambda lam *)
      (*   Lam_stats_util.pp_arities arity *)
      (* ; *)
      annotate meta rec_flag ident  arity lam;
      collect l
    | x -> 
        collect x ;
        if Ident_set.mem ident meta.export_idents then 
          annotate meta rec_flag ident (Lam_stats_util.get_arity meta x ) lam


  and collect  (lam : Lambda.lambda)  =
    match lam with 
    (* | Lprim (Pgetglobal ident,[]) *)
    (*   -> *)
    (*     if not @@ Ident.is_predef_exn ident  then *)
    (*       Lam_util.add_required_module ident meta *)
        (** TODO: 
            how about module aliases..
            record dependency
            --- tricky -- if we inlining, 
            is it safe to remove it? probably not...
         *)
    | Lconst _ -> ()
    | Lvar _ -> ()
    | Lapply(l1, ll, _) ->
        collect  l1; List.iter collect  ll
    | Lfunction(_kind, params, l) -> (* functor ? *)
        List.iter (fun p -> Hashtbl.add meta.ident_tbl p Parameter ) params;
        collect  l
    | Llet (kind,ident,arg,body) -> 
        collect_bind Non_rec kind ident arg ; collect body
    | Lletrec (bindings, body) -> 
        List.iter (fun (ident,arg) -> collect_bind Rec  Strict ident arg ) bindings;
        collect body
    | Lprim(_p, ll) -> List.iter collect  ll
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
        Hash_set.add meta.exit_codes code;
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
    | Levent(l, _) -> collect  l
    | Lifused(_, l) -> collect  l in collect lam 



let count_alias_globals 
    env 
    filename
    export_idents
    (lam : Lambda.lambda) : Lam_stats.meta =
  let meta : Lam_stats.meta = 
    {alias_tbl = Hashtbl.create 31 ; 
     ident_tbl = Hashtbl.create 31;
     exit_codes = Hash_set.create 31 ;
     exports =  export_idents;
     required_modules = [] ;
     filename;
     env;
     export_idents = Lam_util.ident_set_of_list export_idents; 
   } in 
  collect_helper  meta lam ; 
  meta
