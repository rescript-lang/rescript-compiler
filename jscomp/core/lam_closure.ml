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





type stats = 
  { 
    top : bool ; 
    (* all appearances are in the top,  substitution is fine 
       whether it is pure or not
       {[
         (fun x y          
           ->  x + y + (f x )) (32) (console.log('hi'), 33)
       ]}       
       since in ocaml, the application order is intentionally undefined, 
       note if [times] is not one, this field does not make sense       
    *)    
    times : int ; 
  }
type position = 
  { top  : bool ; 
    loop : bool 
  }

let no_substitute : position = { top = false; loop = true }
let fresh_env : position = {top = true; loop = false }
let fresh_stats  : stats = { top = true; times = 0 }

let param_map_of_list lst : stats Ident_map.t = 
  Ext_list.fold_left lst Ident_map.empty (fun l acc -> Ident_map.add l fresh_stats acc) 

(** Sanity check, remove all varaibles in [local_set] in the last pass *)  

let loop_use = 100 (** Used in loop, huge punishment *)

(**
   [param_stats = free_variables exports param_stats lam] 
   This function tries to do more than detect free variable of [lam],  
   given [param_stats] it tries to return a new stats with updated usage of 
   recorded params and unbound parameters
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
  (* base don the envrionmet, recoring the use cases of arguments *)
  let use_at_position {top; loop} v = 
    (* relies on [identifier] uniquely bound *)    
    if not (Ident_set.mem v !local_set) then 
      fv := Ident_map.adjust 
          v
          (fun _ -> {top; times = if loop then loop_use else 1})
          (fun v -> {times = if loop then loop_use else v.times + 1 ; top = v.top && top})
          !fv in
  let new_position_after_lam lam (env : position) : position = 
    if env.top then 
      if Lam_analysis.no_side_effects lam 
      then env 
      (* no side effect, if argument has no side effect and used only once we can simply do the replacement *)
      else { env with top = false}
    else env  in    
  let rec iter (top : position) (lam : Lam.t) =
    match lam with 
    | Lvar v -> use_at_position top v 
    | Lconst _ -> ()
    | Lapply {fn; args; _} ->
      iter top  fn; 
      let top = new_position_after_lam fn top in
      Ext_list.iter args (fun lam -> iter top lam ) 
    | Lprim {args ; _} -> 
      (* Check: can top be propoaged for all primitives *)
      Ext_list.iter args (iter top) 
    | Lglobal_module _ -> ()
    | Lfunction{ params; body} ->
      local_add_list params;
      iter no_substitute body (* Do we need continue *)
    | Llet(_, id, arg, body) ->
      iter top arg;
      local_add id ;  
      iter no_substitute body
    | Lletrec(decl, body) ->
      local_set := Ext_list.fold_left decl !local_set  (fun (id, _) acc -> 
          Ident_set.add id acc) ;        
      Ext_list.iter decl (fun (_, exp) -> iter no_substitute exp);
      iter no_substitute body
    | Lswitch(arg, 
              ({sw_consts; 
                sw_blocks; 
                sw_failaction;
                sw_numconsts;
                sw_numblocks
               })) ->
      iter top arg; 
      let top = new_position_after_lam arg top  in       
      List.iter (fun (_, case) -> iter top case) sw_consts;
      List.iter (fun (_, case) -> iter top  case) sw_blocks;

      begin match sw_failaction with 
        | None -> ()
        | Some x ->
          if  
            sw_numconsts
            ||
            sw_numblocks
          then
            iter top x 
          else
            iter no_substitute x
      end

    | Lstringswitch (arg,cases,default) ->
      iter top arg ;
      let top = new_position_after_lam arg top  in       
      List.iter (fun (_,act) -> iter top  act) cases ;
      begin match default with 
        | None -> ()
        | Some x -> iter top x 
      end
    | Lstaticraise (_,args) ->
      List.iter (iter no_substitute ) args
    | Lstaticcatch(e1, (_,vars), e2) ->
      iter no_substitute  e1; 
      local_add_list vars;       
      iter no_substitute e2
    | Ltrywith(e1, exn, e2) ->
      iter top  e1; iter no_substitute  e2
    | Lifthenelse(e1, e2, e3) ->
      iter top e1; 
      let top = new_position_after_lam e1 top  in
      iter top e2; iter top e3
    | Lsequence(e1, e2) ->
      iter top e1; iter no_substitute e2
    | Lwhile(e1, e2) ->
      iter no_substitute e1; iter no_substitute e2 (* in the loop, no substitution any way *)
    | Lfor(v, e1, e2, dir, e3) ->
      local_add v ; 
      iter no_substitute e1; iter no_substitute e2; iter no_substitute e3
    | Lassign(id, e) ->
      use_at_position top  id ; 
      iter top e
    | Lsend (_k, met, obj, args, _) ->
      iter no_substitute met ; 
      iter no_substitute obj;
      List.iter (iter no_substitute) args in
  iter fresh_env  lam ; 
  !fv 


let is_closed_by set lam = 
  Ident_map.is_empty (free_variables set (Ident_map.empty ) lam   )


(** A bit consverative , it should be empty *)
let is_closed  lam = 
  Ident_map.for_all (fun k _ -> Ident.global k)
    (free_variables Ident_set.empty Ident_map.empty lam)  


let is_closed_with_map exports params body = 
  let param_map = free_variables exports (param_map_of_list params) body in
  let old_count = List.length params in
  let new_count = Ident_map.cardinal param_map in
  (old_count  = new_count, param_map)



