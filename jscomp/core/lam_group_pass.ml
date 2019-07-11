(*type bindings = (Ident.t * Lam.t) list let scc (groups : bindings) (lam :
  Lam.t) (body : Lam.t) (cont : bindings -> Lam.t-> Lam.t) = let domain =
  Ordered_hash_map.create 3 in List.iter (fun (x,lam) -> Ordered_hash_map.add
  domain x lam) groups ; let int_mapping = Ordered_hash_map.to_sorted_array
  domain in let node_vec = Array.make (Array.length int_mapping) (Int_vec.empty
  ()) in Ordered_hash_map.iter ( fun id lam key_index -> let base_key =
  node_vec.(key_index) in let free_vars = Lam_util.free_variables lam in
  Ident_set.iter (fun x -> let key = Ordered_hash_map.find domain x in if key
  >= 0 then Int_vec.push key base_key ) free_vars ) domain; let clusters =
  Ext_scc.graph node_vec in if Int_vec_vec.length clusters <= 1 then lam else
  Int_vec_vec.fold_right (fun (v : Int_vec.t) acc -> cont
  (Int_vec.map_into_list (fun i -> let id = int_mapping.(i) in let lam =
  Ordered_hash_map.find_value domain id in (id,lam) ) v ) acc ) clusters body *)

let rec scc_pass (lam : Lam.t) =
  let lam = Lam.inner_map lam scc_pass in
  match lam with
  | Lletrec (bindings, body) -> Lam_scc.scc bindings lam body
  | _ -> lam
