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








let simplify_alias 
    (meta : Lam_stats.t)
    (lam : Lam.t) 
  :  Lam.t  = 

  let rec simpl  (lam : Lam.t) : Lam.t = 
    match lam with 
    | Lvar v ->
      begin match (Ident_hashtbl.find_opt meta.alias_tbl v) with
      | None -> lam
      | Some v ->
        if Ident.persistent v then 
          Lam.global_module v 
        else 
         Lam.var v 
        (* This is wrong
            currently alias table has info 
            include -> Array

            however, (field id Array/xx) 
            does not result in a reduction, so we 
            still pick the old one (field id include)
            which makes dead code elimination wrong
         *)
      end
      (* GLOBAL module needs to be propogated *)
    | Llet (kind, k, (Lglobal_module i as g), l )
           -> 
      (* This is detection of global MODULE inclusion
          we need track all global module aliases, when it's
          passed as a parameter(escaped), we need do the expansion
          since global module access is not the same as local module
          TODO: 
          since we aliased k, so it's safe to remove it?
          no, we should not shake away code just by [Ident_set.mem k meta.export_idents ]
          in that case, we should provide strong guarantee that all [k] will be substitued
      *)
      let v = simpl l in
      Lam.let_ kind k g v
        (* in this case it is preserved, but will still be simplified 
            for the inner expression
        *)
      
    | Lprim {primitive = (Pfield (i,_) as primitive); args =  [arg]; loc} -> 
      (* ATTENTION: 
         Main use case, we should detect inline all immutable block .. *)
      begin match  simpl  arg with 
      | Lglobal_module g 
      ->    
        Lam.prim 
          ~primitive:(Pfield(i,Lambda.Fld_na))
          ~args:[Lam.global_module g ]
          loc
      | Lvar v as l-> 
        Lam_util.field_flatten_get (fun _ -> Lam.prim ~primitive ~args:[l] loc )
         v  i meta.ident_tbl 
      | _ ->  
        Lam.prim ~primitive ~args:[simpl arg] loc 
      end
    | Lglobal_module _ -> lam 
    | Lprim {primitive; args; loc } 
      -> Lam.prim ~primitive ~args:(Ext_list.map simpl  args) loc
      
    | Lifthenelse(Lvar id as l1, l2, l3) 
      -> 
      begin match Ident_hashtbl.find_opt meta.ident_tbl id with 
      | Some (ImmutableBlock ( _, Normal))
      | Some (MutableBlock _  )
        -> simpl l2 
      | Some (ImmutableBlock ( [| SimpleForm l |]  , x) )
        -> 
        let l1 = 
          match x with 
          | Null 
            -> Lam.not_ (Location.none) ( Lam.prim ~primitive:Pis_null

            ~args:[l] Location.none) 
          | Undefined 
            -> 
            Lam.not_  Location.none (Lam.prim ~primitive:Pis_undefined ~args:[l] Location.none)
          | Null_undefined
            -> 
            Lam.not_ Location.none
              ( Lam.prim ~primitive:Pis_null_undefined  ~args:[l] Location.none) 
          | Normal ->  l1 
        in 
        Lam.if_ l1 (simpl l2) (simpl l3)
      | Some _
      | None -> Lam.if_ l1 (simpl l2) (simpl l3)
      end
    | Lifthenelse (l1, l2, l3) -> 
        Lam.if_ (simpl  l1) (simpl  l2) (simpl  l3)

    | Lconst _ -> lam
    | Llet(str, v, l1, l2) ->
      Lam.let_ str v (simpl l1) (simpl l2 )
    | Lletrec(bindings, body) ->
      let bindings = Ext_list.map (fun (k,l) ->  (k, simpl l) ) bindings in 
      Lam.letrec bindings (simpl body) 
 
    (* complicated 
        1. inline this function
        2. ...
        exports.Make=
        function(funarg)
      {var $$let=Make(funarg);
        return [0, $$let[5],... $$let[16]]}
    *)      
    | Lapply{fn = 
               Lprim {primitive = Pfield (index, _) ;
                      args = [ Lglobal_module ident ];
                      _} as l1;
             args; loc ; status} ->
      begin
             match  Lam_compile_env.cached_find_ml_id_pos ident index meta.env with                   
              | {closed_lambda=Some Lfunction{params; body; _} } 
                (** be more cautious when do cross module inlining *)
                when
                  ( Ext_list.same_length params args &&
                    List.for_all (fun (arg : Lam.t) ->
                        match arg with 
                        | Lvar p -> 
                          begin 
                            match Ident_hashtbl.find_opt meta.ident_tbl p with
                            | Some v  -> v <> Parameter
                            | None -> true 
                          end
                        |  _ -> true 
                      ) args) -> 
                simpl @@
                Lam_beta_reduce.propogate_beta_reduce
                  meta params body args
              | _ -> 
                Lam.apply (simpl l1) (Ext_list.map simpl args) loc status
            

      end
    (* Function inlining interact with other optimizations...

        - parameter attributes
        - scope issues 
        - code bloat 
    *)      
    | Lapply{fn = (Lvar v as fn);  args; loc ; status} ->
      (* Check info for always inlining *)

      (* Ext_log.dwarn __LOC__ "%s/%d" v.name v.stamp;     *)
      let normal () = Lam.apply ( simpl fn) (Ext_list.map simpl args) loc status in
      begin 
        match Ident_hashtbl.find_opt meta.ident_tbl v with
        | Some (FunctionId {lambda = Some(Lfunction {params; body} as _m,
                    rec_flag)
                     })
          -> 
        
          if Ext_list.same_length args params (* && false *)
          then               
            if Lam_inline_util.maybe_functor v.name  
              (* && (Ident_set.mem v meta.export_idents) && false *)
            then 
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
            if (* Lam_analysis.size body < Lam_analysis.small_inline_size *)
              Lam_analysis.ok_to_inline_fun_when_app ~body params args 
            then 

                (* let param_map =  *)
                (*   Lam_analysis.free_variables meta.export_idents  *)
                (*     (Lam_analysis.param_map_of_list params) body in *)
                (* let old_count = List.length params in *)
                (* let new_count = Ident_map.cardinal param_map in *)
                let param_map = 
                  Lam_closure.is_closed_with_map 
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
                | _ -> normal ()
              else 
                normal ()
          else
            normal ()
        | Some _
        | None -> normal ()

      end

    | Lapply{ fn = Lfunction{ function_kind = Curried ; params; body}; args; _}
      when  Ext_list.same_length params args ->
      simpl (Lam_beta_reduce.propogate_beta_reduce meta params body args)
    (* | Lapply{ fn = Lfunction{function_kind =  Tupled;  params; body};  *)
    (*          args = [Lprim {primitive = Pmakeblock _; args; _}]; _} *)
    (*   (\** TODO: keep track of this parameter in ocaml trunk, *)
    (*       can we switch to the tupled backend? *)
    (*   *\) *)
    (*   when  Ext_list.same_length params args -> *)
    (*   simpl (Lam_beta_reduce.propogate_beta_reduce meta params body args) *)

    | Lapply {fn = l1; args =  ll;  loc ; status} ->
      Lam.apply (simpl  l1) (Ext_list.map simpl  ll) loc status
    | Lfunction {arity; function_kind; params; body = l}
      -> Lam.function_ ~arity ~function_kind ~params  ~body:(simpl  l)
    | Lswitch (l, {sw_failaction; 
                   sw_consts; 
                   sw_blocks;
                   sw_numblocks;
                   sw_numconsts;
                  }) ->
      Lam.switch (simpl  l)
               {sw_consts = 
                  Ext_list.map (fun (v, l) -> v, simpl  l) sw_consts;
                sw_blocks = Ext_list.map (fun (v, l) -> v, simpl  l) sw_blocks;
                sw_numconsts = sw_numconsts;
                sw_numblocks = sw_numblocks;
                sw_failaction = 
                  begin 
                    match sw_failaction with 
                    | None -> None
                    | Some x -> Some (simpl x)
                  end}
    | Lstringswitch(l, sw, d) ->
      Lam.stringswitch (simpl  l )
                    (Ext_list.map (fun (i, l) -> i,simpl  l) sw)
                    (match d with
                     | Some d -> Some (simpl d )
                     | None -> None)
    | Lstaticraise (i,ls) -> 
      Lam.staticraise i (Ext_list.map simpl  ls)
    | Lstaticcatch (l1, ids, l2) -> 
      Lam.staticcatch (simpl  l1) ids (simpl  l2)
    | Ltrywith (l1, v, l2) -> Lam.try_ (simpl  l1) v (simpl  l2)
    | Lsequence(l1, l2)
      -> Lam.seq (simpl  l1) (simpl  l2)
    | Lwhile(l1, l2)
      -> Lam.while_ (simpl  l1) (simpl l2)
    | Lfor(flag, l1, l2, dir, l3)
      -> 
      Lam.for_ flag (simpl  l1) (simpl  l2) dir (simpl  l3)
    | Lassign(v, l) ->
      (* Lalias-bound variables are never assigned, so don't increase
         v's refsimpl *)
      Lam.assign v (simpl  l)
    | Lsend (u, m, o, ll, v) 
      -> 
      Lam.send u (simpl m) (simpl o) (Ext_list.map simpl ll) v
    | Lifused (v, l) -> Lam.ifused v (simpl  l)
  in 
  simpl lam


