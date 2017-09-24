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











let transitive_closure 
    (initial_idents : Ident.t list) 
    (ident_freevars : Ident_set.t Ident_hashtbl.t) 
  =
  let visited  = Ident_hash_set.create 31 in 
  let rec dfs (id : Ident.t) =
    if Ident_hash_set.mem visited id || Ext_ident.is_js_or_global id  
    then ()
    else 
      begin 
        Ident_hash_set.add visited id;
        match Ident_hashtbl.find_opt ident_freevars id with 
        | None -> 
          Ext_pervasives.failwithf ~loc:__LOC__ "%s/%d not found"  (Ident.name id) (id.Ident.stamp)  
        | Some e -> Ident_set.iter (fun id -> dfs id) e
      end  in 
  List.iter dfs initial_idents;
  visited

let remove export_idents (rest : Lam_group.t list) : Lam_group.t list  = 
  let ident_free_vars :  _ Ident_hashtbl.t = Ident_hashtbl.create 17 in
  (* calculate initial required idents, 
     at the same time, populate dependency set [ident_free_vars]
  *)
  let initial_idents =
    List.fold_left (fun acc (x : Lam_group.t) -> 
        match x with
        | Single(kind, id,lam) ->                   
          begin
            Ident_hashtbl.add ident_free_vars id 
              (Lam.free_variables  lam);
            match kind with
            | Alias | StrictOpt -> acc
            | Strict | Variable -> id :: acc 
          end
        | Recursive bindings -> 
          List.fold_left (fun acc (id,lam) -> 
              Ident_hashtbl.add ident_free_vars id (Lam.free_variables lam);
              match (lam : Lam.t) with
              | Lfunction _ -> acc 
              | _ -> id :: acc
            ) acc bindings 
        | Nop lam ->
          if Lam_analysis.no_side_effects lam then acc
          else 
            (** its free varaibles here will be defined above *)
            Ident_set.fold (fun x acc -> x :: acc )  ( Lam.free_variables lam) acc                
      )  export_idents rest in 
  let visited = transitive_closure initial_idents ident_free_vars in 
  List.fold_left (fun (acc : _ list) (x : Lam_group.t) ->
      match x with 
      | Single(_,id,_) -> 
        if Ident_hash_set.mem visited id  then 
          x :: acc 
        else acc 
      | Nop _ -> x :: acc  
      | Recursive bindings ->
        let b = 
          Ext_list.fold_right (fun ((id,_) as v) acc ->
              if Ident_hash_set.mem visited id then 
                v :: acc 
              else
                acc  
            ) bindings [] in            
        match b with 
        | [] -> acc  
        | _ -> (Recursive b) :: acc
    ) [] rest |> List.rev   

  
