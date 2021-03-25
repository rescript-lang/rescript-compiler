(* Copyright (C) 2015 - 2016 Bloomberg Finance L.P.
 * Copyright (C) 2017 - Hongbo Zhang, Authors of ReScript 
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





(*type bindings = (Ident.t * Lam.t) list
  let scc  (groups :  bindings)  
    (lam : Lam.t)
    (body : Lam.t)
    (cont : bindings -> Lam.t-> Lam.t) =     
  let domain = Ordered_hash_map.create 3 in 
  List.iter (fun (x,lam) -> Ordered_hash_map.add domain x lam) groups ;
  let int_mapping = Ordered_hash_map.to_sorted_array domain in 
  let node_vec = Array.make (Array.length int_mapping) (Vec_int.empty ()) in
  Ordered_hash_map.iter ( fun id lam key_index ->        
      let base_key =  node_vec.(key_index) in 
      let free_vars = Lam_util.free_variables lam in
      Set_ident.iter (fun x ->
          let key = Ordered_hash_map.find domain x in 
          if key >= 0 then 
            Vec_int.push key base_key 
        ) free_vars
    ) domain;
  let clusters = Ext_scc.graph node_vec in 
  if Int_vec_vec.length clusters <= 1 then lam 
  else          
    Int_vec_vec.fold_right (fun  (v : Vec_int.t) acc ->
        cont (Vec_int.map_into_list (fun i -> 
            let id = int_mapping.(i) in 
            let lam  = Ordered_hash_map.find_value domain  id in  
            (id,lam)
        ) v )  acc 
      )  clusters body 

*)

let rec scc_pass (lam : Lam.t) =  
  let lam = Lam.inner_map lam scc_pass in 
  match lam with 
  | Lletrec (bindings, body) -> 
    Lam_scc.scc bindings lam body   
  | _ -> lam  