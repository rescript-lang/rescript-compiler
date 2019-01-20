(* Copyright (C) 2017 Authors of BuckleScript
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





type position = Lam_var_stats.position
type stats = Lam_var_stats.stats

let adjust ( pos : position) (v : Ident.t) (fv : stats Ident_map.t) : stats Ident_map.t = 
  Ident_map.adjust fv v
    (fun v -> 
      let stat = match v with None -> Lam_var_stats.fresh_stats | Some v -> v in 
      Lam_var_stats.update pos stat)
    
    



let param_map_of_list lst : stats Ident_map.t = 
  Ext_list.fold_left lst Ident_map.empty 
    (fun l acc -> Ident_map.add acc l Lam_var_stats.fresh_stats ) 

let sink_pos = Lam_var_stats.sink    
(** Sanity check, remove all varaibles in [local_set] in the last pass *)  


(**
   [param_stats = free_variables exports param_stats lam] 
   This function tries to do more than detect free variable of [lam],  
   given [param_stats] it tries to return a new stats with updated usage of 
   recorded params and unbound parameters

   An enriched version of [free_varaibles] in {!Lam_free_variables}
*)
let free_variables 
    (export_idents : Ident_set.t ) 
    (params : stats Ident_map.t ) 
    (lam : Lam.t )
  : stats Ident_map.t = 
  let fv = ref params in
  let local_set = ref export_idents in
  let local_add k =
    local_set := Ident_set.add k !local_set in
  let local_add_list ks = 
    local_set :=  
      Ext_list.fold_left ks !local_set (fun k acc -> Ident_set.add k acc) in    
  (* base don the envrionmet, recoring the use cases of arguments 
     relies on [identifier] uniquely bound *)    
  let used (cur_pos : position) (v : Ident.t) = 

    if not (Ident_set.mem v !local_set) then 
      fv := adjust cur_pos v !fv in

  let rec iter (top : position) (lam : Lam.t) =
    match lam with 
    | Lvar v -> used top v 
    | Lconst _ -> ()
    | Lapply {fn; args; _} ->
      iter top  fn; 
      let top = Lam_var_stats.new_position_after_lam fn top in
      Ext_list.iter args (fun lam -> iter top lam ) 
    | Lprim {args ; _} -> 
      (* Check: can top be propoaged for all primitives *)
      Ext_list.iter args (iter top) 
    | Lglobal_module _ -> ()
    | Lfunction{ params; body} ->
      local_add_list params;
      iter sink_pos body (* Do we need continue *)
    | Llet(_, id, arg, body) ->
      iter top arg;
      local_add id ;  
      iter sink_pos body
    | Lletrec(decl, body) ->
      local_set := Ext_list.fold_left decl !local_set  (fun (id, _) acc -> 
          Ident_set.add id acc) ;        
      Ext_list.iter decl (fun (_, exp) -> iter sink_pos exp);
      iter sink_pos body
    | Lswitch(arg, 
              ({sw_consts; 
                sw_blocks; 
                sw_failaction;
                sw_numconsts;
                sw_numblocks
               })) ->
      iter top arg; 
      let top = Lam_var_stats.new_position_after_lam arg top  in       
      List.iter (fun (_, case) -> iter top case) sw_consts;
      List.iter (fun (_, case) -> iter top  case) sw_blocks;
      (match sw_failaction with 
       | None -> ()
       | Some x ->
         if  sw_numconsts || sw_numblocks
         then iter top x 
         else iter sink_pos x)      
    | Lstringswitch (arg,cases,default) ->
      iter top arg ;
      let top = Lam_var_stats.new_position_after_lam arg top  in       
      List.iter (fun (_,act) -> iter top  act) cases ;
      (match default with 
       | None -> ()
       | Some x -> iter top x )      
    | Lstaticraise (_,args) ->
      List.iter (iter sink_pos ) args
    | Lstaticcatch(e1, (_,vars), e2) ->
      iter sink_pos  e1; 
      local_add_list vars;       
      iter sink_pos e2
    | Ltrywith(e1, exn, e2) ->
      iter top  e1; iter sink_pos  e2
    | Lifthenelse(e1, e2, e3) ->
      iter top e1; 
      let top = Lam_var_stats.new_position_after_lam e1 top  in
      iter top e2; iter top e3
    | Lsequence(e1, e2) ->
      iter top e1; iter sink_pos e2
    | Lwhile(e1, e2) ->
      iter sink_pos e1; iter sink_pos e2 (* in the loop, no substitution any way *)
    | Lfor(v, e1, e2, dir, e3) ->
      local_add v ; 
      iter sink_pos e1; iter sink_pos e2; iter sink_pos e3
    | Lassign(id, e) ->
      used top  id ; 
      iter top e
    | Lsend (_k, met, obj, args, _) ->
      iter sink_pos met ; 
      iter sink_pos obj;
      List.iter (iter sink_pos) args in
  iter Lam_var_stats.fresh_env  lam ; 
  !fv 


let is_closed_by (set : Ident_set.t) (lam : Lam.t) : bool = 
  Ident_map.is_empty (free_variables set (Ident_map.empty ) lam   )


(** A bit consverative , it should be empty *)
let is_closed  lam = 
  Ident_map.for_all (fun k _ -> Ident.global k)
    (free_variables Ident_set.empty Ident_map.empty lam)  


let is_closed_with_map 
  (exports : Ident_set.t) 
  (params : Ident.t list) 
  (body : Lam.t) : bool * stats Ident_map.t = 
  let param_map = free_variables exports (param_map_of_list params) body in
  let old_count = List.length params in
  let new_count = Ident_map.cardinal param_map in
  (old_count  = new_count, param_map)



