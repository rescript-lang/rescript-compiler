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



let simplify_alias 
    (meta : Lam_stats.meta)
    (lam : Lambda.lambda) 
  :  Lambda.lambda  = 

  let rec simpl  (lam : Lambda.lambda) : Lambda.lambda = 
    match lam with 
    | Lvar v -> 
      (* GLOBAL module needs to be propogated *)
      (try Lvar (Hashtbl.find meta.alias_tbl v) with Not_found -> lam )
    | Llet(kind, k, (Lprim (Pgetglobal i,[]) as g), l ) -> 
      (* This is detection of MODULE ALIAS 
          we need track all global module aliases, when it's
          passed as a parameter(escaped), we need do the expansion
          since global module access is not the same as local module
          TODO: 
          since we aliased k, so it's safe to remove it?
      *)
      let v = simpl l in
      if Ident_set.mem k meta.export_idents 
      then 
        Llet(kind, k, g, v) 
        (* in this case it is preserved, but will still be simplified 
            for the inner expression
        *)
      else v
    | Lprim (Pfield i, [Lvar v]) -> 
      (* ATTENTION: 
         Main use case, we should detect inline all immutable block .. *)
      Lam_util.get lam v  i meta.ident_tbl 
    | Lconst _ -> lam
    | Llet(str, v, l1, l2) ->
      Llet(str, v, simpl l1, simpl l2 )
    | Lletrec(bindings, body) ->
      let bindings = List.map (fun (k,l) ->  (k, simpl l) ) bindings in 
      Lletrec(bindings, simpl body) 
    | Lprim(prim, ll) -> Lprim(prim, List.map simpl  ll)

    (* complicated 
        1. inline this function
        2. ...
        exports.Make=
        function(funarg)
      {var $$let=Make(funarg);
        return [0, $$let[5],... $$let[16]]}
    *)      
    | Lapply(Lprim(Pfield index , [Lprim (Pgetglobal ident, [])]) as l1,
                                                                     args, info) ->
      begin
        Lam_compile_env.find_and_add_if_not_exist (ident,index) meta.env
          ~not_found:(fun _ -> assert false)
          ~found:(fun i ->
              match i with
              | {closed_lambda=Some Lfunction(Curried, params, body) } 
                (** be more cautious when do cross module inlining *)
                when
                  ( Ext_list.same_length params args &&
                    List.for_all (fun (arg : Lambda.lambda) ->
                        match arg with 
                        | Lvar p -> 
                          begin 
                            try Hashtbl.find meta.ident_tbl p != Parameter
                            with Not_found -> true
                          end
                        |  _ -> true 
                      ) args) -> 
                simpl @@
                Lam_beta_reduce.propogate_beta_reduce
                  meta params body args
              | _ -> Lapply (simpl l1, List.map simpl args, info)
            )

      end
    (* Function inlining interact with other optimizations...

        - parameter attributes
        - scope issues 
        - code bloat 
    *)      
    | Lapply((Lvar v as l1), args, info) -> (* Check info for always inlining *)

      (* Ext_log.dwarn __LOC__ "%s/%d" v.name v.stamp;     *)

      begin 
        match Hashtbl.find meta.ident_tbl v with
        | Function {lambda = (Lfunction(_, params, body) as _m);
                    rec_flag;                     
                    _ }
          -> 
          let lam_size = Lam_analysis.size body in            
          if Ext_list.same_length args params 
          then               
            if Lam_inline_util.maybe_functor v.name  then 
              (* && not (List.mem v meta.export_idents) *)
              (* TODO: check l1 if it is exported, 
                 if so, maybe not since in that case, 
                 we are going to have two copy?
              *)

              (* Check: recursive applying may result in non-termination *)
              begin
                (* Ext_log.dwarn __LOC__ "beta .. %s/%d" v.name v.stamp ; *)
                simpl (Lam_beta_reduce.propogate_beta_reduce meta params body args) 
              end
            else 
              if lam_size < Lam_analysis.small_inline_size then 

                (* let param_map =  *)
                (*   Lam_analysis.free_variables meta.export_idents  *)
                (*     (Lam_analysis.param_map_of_list params) body in *)
                (* let old_count = List.length params in *)
                (* let new_count = Ident_map.cardinal param_map in *)
                let param_map = 
                  Lam_analysis.is_closed_with_map 
                    meta.export_idents params body in
                let is_export_id = Ident_set.mem v meta.export_idents in
                match is_export_id, param_map with 
                | false, (_, param_map)
                | true, (true, param_map) -> 
                  if rec_flag = Rec then               
                    begin
                      (* Ext_log.dwarn __LOC__ "beta rec.. %s/%d" v.name v.stamp ; *)
                      (* Lam_beta_reduce.propogate_beta_reduce meta params body args *)
                      Lam_beta_reduce.propogate_beta_reduce_with_map meta param_map params body args
                    end
                  else 
                    begin
                      (* Ext_log.dwarn __LOC__ "beta  nonrec..[%d] [%a]  %s/%d"  *)
                      (*   (List.length args)  *)
                      (*   Printlambda.lambda body                      *)
                      (*   v.name v.stamp ; *)
                      simpl (Lam_beta_reduce.propogate_beta_reduce_with_map meta param_map params body args)

                    end
                | _ -> 
                  Lapply ( simpl l1, List.map simpl args, info)
              else 
                begin
                  (* Ext_log.dwarn __LOC__ "%s/%d: %d "  *)
                  (*   v.name v.stamp lam_size *)
                  (* ;     *)
                  Lapply ( simpl l1, List.map simpl args, info)
                end
          else
            begin
              (* Ext_log.dwarn __LOC__ "%d vs %d " (List.length args) (List.length params); *)
              Lapply ( simpl l1, List.map simpl args, info)
            end

        | _ -> 
          begin
            (* Ext_log.dwarn __LOC__ "%s/%d -- no source " v.name v.stamp;     *)
            Lapply ( simpl l1, List.map simpl args, info)
          end
        | exception Not_found -> 
            (* Ext_log.dwarn __LOC__ "%s/%d -- not found " v.name v.stamp;     *)
          Lapply ( simpl l1, List.map simpl args, info)
      end

    | Lapply(Lfunction(Curried, params, body), args, _)
      when  Ext_list.same_length params args ->
      simpl (Lam_beta_reduce.propogate_beta_reduce meta params body args)
    | Lapply(Lfunction(Tupled, params, body), [Lprim(Pmakeblock _, args)], _)
      (** TODO: keep track of this parameter in ocaml trunk,
          can we switch to the tupled backend?
      *)
      when  Ext_list.same_length params args ->
      simpl (Lam_beta_reduce.propogate_beta_reduce meta params body args)

    | Lapply (l1, ll, info) ->
      Lapply (simpl  l1, List.map simpl  ll,info)
    | Lfunction (kind, params, l) 
      -> Lfunction (kind, params , simpl  l)
    | Lswitch (l, {sw_failaction; 
                   sw_consts; 
                   sw_blocks;
                   sw_numblocks;
                   sw_numconsts;
                  }) ->
      Lswitch (simpl  l,
               {sw_consts = 
                  List.map (fun (v, l) -> v, simpl  l) sw_consts;
                sw_blocks = List.map (fun (v, l) -> v, simpl  l) sw_blocks;
                sw_numconsts = sw_numconsts;
                sw_numblocks = sw_numblocks;
                sw_failaction = 
                  begin 
                    match sw_failaction with 
                    | None -> None
                    | Some x -> Some (simpl x)
                  end})
    | Lstringswitch(l, sw, d) ->
      Lstringswitch(simpl  l ,
                    List.map (fun (i, l) -> i,simpl  l) sw,
                    begin 
                      match d with
                      | Some d -> Some (simpl d )
                      | None -> None
                    end)
    | Lstaticraise (i,ls) -> Lstaticraise(i, List.map (simpl ) ls)
    | Lstaticcatch (l1, (i,x), l2) -> Lstaticcatch(simpl  l1, (i,x), simpl  l2)
    | Ltrywith (l1, v, l2) -> Ltrywith(simpl  l1,v, simpl  l2)
    | Lifthenelse (l1, l2, l3) -> Lifthenelse(simpl  l1, simpl  l2, simpl  l3)
    | Lsequence (Lprim (Pgetglobal (id),[]), l2)
      when Lam_compile_env.is_pure (Lam_module_ident.of_ml id) meta.env 
      -> simpl l2
    | Lsequence(l1, l2) -> Lsequence (simpl  l1, simpl  l2)
    | Lwhile(l1, l2) -> Lwhile (simpl  l1, simpl l2)
    | Lfor(flag, l1, l2, dir, l3) -> Lfor (flag,simpl  l1, simpl  l2, dir, simpl  l3)
    | Lassign(v, l) ->
      (* Lalias-bound variables are never assigned, so don't increase
         v's refsimpl *)
      Lassign (v,simpl  l)
    | Lsend (u, m, o, ll, v) -> Lsend (u, simpl m, simpl o, List.map simpl ll,v)
    | Levent (l, event) -> Levent (simpl  l, event)
    | Lifused (v, l) -> Lifused (v,simpl  l)
  in 
  simpl lam
