(* Copyright (C) 2018 Authors of BuckleScript
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



(**
    [hit_mask mask lambda] iters through the lambda
    set the bit of corresponding [id] if [id] is hit.
    As an optimization step if [mask_check_all_hit],
    there is no need to iter such lambda any more
*)
let hit_mask ( mask : Hash_set_ident_mask.t) (l : Lam.t) : bool =
  let rec 
    hit_opt (x : Lam.t option) = 
    match x with 
    | None -> false 
    | Some a -> hit a
    and hit_var (id : Ident.t) = Hash_set_ident_mask.mask_check_all_hit id mask
    and hit_list_snd : 'a. ('a * Lam.t ) list -> bool = fun x ->    
      Ext_list.exists_snd x hit
    and hit_list xs = List.exists hit xs 
    and hit (l : Lam.t) =
    match l  with
    | Lvar id -> hit_var id
    | Lassign(id, e) ->
      hit_var id || hit e
    | Lstaticcatch(e1, (_,vars), e2) ->
      hit e1 || hit e2
    | Ltrywith(e1, exn, e2) ->
      hit e1 || hit e2
    | Lfunction{body;params} ->
      hit body
    | Llet(str, id, arg, body) ->
      hit arg || hit body
    | Lletrec(decl, body) ->
      hit body ||
      hit_list_snd decl
    | Lfor(v, e1, e2, dir, e3) ->
      hit e1 || hit e2 || hit e3
    | Lconst _ -> false
    | Lapply{fn; args; _} ->
      hit fn || hit_list args
    | Lglobal_module id (* playsafe *)
      -> false
    | Lprim {args; _} ->
      hit_list args
    | Lswitch(arg, sw) ->
      hit arg ||
      hit_list_snd sw.sw_consts ||
      hit_list_snd sw.sw_blocks ||
      hit_opt sw.sw_failaction
    | Lstringswitch (arg,cases,default) ->
      hit arg ||
      hit_list_snd cases ||
      hit_opt default
    | Lstaticraise (_,args) ->
      hit_list args
    | Lifthenelse(e1, e2, e3) ->
      hit e1 || hit e2 || hit e3
    | Lsequence(e1, e2) ->
      hit e1 || hit e2
    | Lwhile(e1, e2) ->
      hit e1 || hit e2
    | Lsend (k, met, obj, args, _) ->
      hit met || hit obj || hit_list args  
  in hit l



 type bindings = (Ident.t * Lam.t) list


let preprocess_deps (groups : bindings) : _ * Ident.t array * Int_vec.t array   =
  let len = List.length groups in
  let domain : _ Ordered_hash_map_local_ident.t =
    Ordered_hash_map_local_ident.create len in
  let mask = Hash_set_ident_mask.create len in
  List.iter (fun (x,lam) ->
      Ordered_hash_map_local_ident.add domain x lam;
      Hash_set_ident_mask.add_unmask mask x;
    ) groups ;
  let int_mapping = Ordered_hash_map_local_ident.to_sorted_array domain in
  let node_vec = Array.make (Array.length int_mapping) (Int_vec.empty ()) in
  domain
  |> Ordered_hash_map_local_ident.iter ( fun id lam key_index ->
      let base_key =  node_vec.(key_index) in
      ignore (hit_mask mask lam) ;
      mask |> Hash_set_ident_mask.iter_and_unmask (fun ident hit  ->
          if hit then
            begin
              let key = Ordered_hash_map_local_ident.rank domain ident in
              Int_vec.push key base_key;
            end
        );

    ) ;
  domain, int_mapping , node_vec


let is_function_bind (_, (x : Lam.t)) =
  match x with
  | Lfunction _ -> true
  | _ -> false

let sort_single_binding_group (group : bindings) =
  if Ext_list.for_all group  is_function_bind then group
  else
    List.sort (fun (_,lama) (_,lamb) ->
        match (lama : Lam.t), (lamb : Lam.t) with
        | Lfunction _, Lfunction _ ->  0
        | Lfunction _ , _ -> -1
        | _, Lfunction _ -> 1
        | _,_ -> 0
      ) group

(** TODO: even for a singleton recursive function, tell whehter it is recursive or not ? *)
let scc_bindings (groups : bindings) : bindings list =
  match groups with
  | [ _ ] -> [ sort_single_binding_group groups ]
  | _ ->
    let domain, int_mapping, node_vec = preprocess_deps groups in
    let clusters : Int_vec_vec.t = Ext_scc.graph node_vec in
    if Int_vec_vec.length clusters <= 1 then [ sort_single_binding_group groups]
    else
      Int_vec_vec.fold_right (fun  (v : Int_vec.t) acc ->
          let bindings =
            Int_vec.map_into_list (fun i ->
                let id = int_mapping.(i) in
                let lam  = Ordered_hash_map_local_ident.find_value domain  id in
                (id,lam)
              ) v  in
          sort_single_binding_group bindings :: acc
        )  clusters []
(* single binding, it does not make sense to do scc,
   we can eliminate {[ let rec f x = x + x  ]}, but it happens rarely in real world
*)
let scc  (groups :  bindings)  ( lam : Lam.t) ( body : Lam.t)
  =
  begin match groups with
    | [ (id,bind) ] ->
      if Lam_hit.hit_variable id bind
      then
        lam
      else Lam.let_ Strict id bind body
    | _ ->
      let (domain, int_mapping, node_vec)  = preprocess_deps groups in
      let clusters = Ext_scc.graph node_vec in
      if Int_vec_vec.length clusters <= 1 then lam
      else
        Int_vec_vec.fold_right (fun  (v : Int_vec.t) acc ->
            let bindings =
              Int_vec.map_into_list (fun i ->
                  let id = int_mapping.(i) in
                  let lam  = Ordered_hash_map_local_ident.find_value domain  id in
                  (id,lam)
                ) v  in
            match bindings with
            | [ id,lam ] ->
              let base_key = Ordered_hash_map_local_ident.rank domain id in
              if Int_vec_util.mem base_key node_vec.(base_key) then
                Lam.letrec bindings acc
              else  Lam.let_ Strict id lam acc
            | _ ->
              Lam.letrec bindings  acc
          )  clusters body
  end


