(* Copyright (C) 2020- Authors of BuckleScript 
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

type lam = Lambda.lambda

let same_action (x : lam) (y : lam) = 
  match Lambda.make_key x , Lambda.make_key y with 
  | Some x, Some y -> x = y
  | Some _, None 
  | None, Some _
  | None, None -> false
module Map_lambda = struct 
  open Map_gen  
  type key = 
    | Bottom of int   
    | Normalized of lam   

  let bottom_id = ref (-1)  
  let bottom () = incr bottom_id ; Bottom !bottom_id 
  let compare_key (x : key) (y : key) = Pervasives.compare x y  
  let old_order = ref 1 
  let next_id () = 
    incr old_order; !old_order  
  let rec adjust (tree : _ Map_gen.t as 'a) x replace  : 'a = 
    match tree with 
    | Empty ->
      singleton x (replace None)
    | Leaf {k ; v} -> 
      let c = compare_key x k in 
      if c = 0 then singleton x (replace (Some v)) else 
      if c < 0 then 
        Map_gen.unsafe_two_elements x (replace None) k v   
      else
        Map_gen.unsafe_two_elements k v x (replace None)   
    | Node ({l; k ; r} as tree) ->
      let c = compare_key x k in
      if c = 0 then
        Map_gen.unsafe_node x (replace  (Some tree.v)) l r tree.h
      else if c < 0 then
        bal (adjust l x  replace ) k tree.v r
      else
        bal l k tree.v (adjust r x  replace )

  
  let of_list (int_lambda_list : (int * (string * lam)) list) : (key, (int * string) list * lam * int ) t= 
    Ext_list.fold_left int_lambda_list  empty (fun acc (hash,(name,lam)) -> 
        let key = 
          match Lambda.make_key lam with 
          | None -> bottom ()
          | Some key -> Normalized key in 
        adjust acc key (function 
          | None -> [hash, name], lam, next_id ()
          | Some (acc,action,stamp) -> (hash,name) :: acc, action, stamp
      ))
  let rec values_aux s acc = 
    match s with 
    | Empty -> acc
    | Leaf {v} -> v :: acc 
    | Node {l;v;r} ->      
       values_aux l (v ::values_aux r acc)         
  let values s : ((int * string) list * lam) list = 
      Ext_list.sort_via_arrayf ( values_aux s []) 
      (fun (a,b,_) -> (a,b))
      (fun (_,_,d0) (_,_,d1) -> compare d0 d1)
end 

let make_test_sequence_variant_constant_2 
  (fail : lam option) (arg : lam) 
  (int_lambda_list : (int * (string * lam) ) list) : lam =
  let int_lambda_list : ((int * string) list * lam) list = Map_lambda.(values (of_list int_lambda_list)) in 
  match int_lambda_list, fail with 
  | (_, act) :: rest, None 
    -> 
    Ext_list.fold_right rest act (fun (hash_names,act1) acc -> 
        let predicate  : lam =
          match hash_names with 
          | (hash,name):: rest ->  
              let init : lam = Lprim(Pintcomp Ceq, 
                [arg; Lconst ((Const_pointer (hash, Pt_variant{name})))],
                Location.none) in 
                Ext_list.fold_left rest init (fun acc (hash,name) -> 
                  Lambda.Lprim 
                    (Psequor , 
                     [acc ;
                     Lprim(Pintcomp Ceq, 
                        [arg; 
                        Lconst ((Const_pointer (hash, Pt_variant{name})))],
                                  Location.none)], Location.none)
                )
          | _ -> assert false      
         in    
        Lifthenelse (predicate,
                       act1, acc
                    ))
| rest, Some act ->                     
  Ext_list.fold_right rest act (fun (hash_names,act1) acc -> 
      let predicate  : lam =
        match hash_names with 
        | (hash,name):: rest ->  
          let init : lam = Lprim(Pintcomp Ceq, 
                                 [arg; Lconst ((Const_pointer (hash, Pt_variant{name})))],
                                 Location.none) in 
          Ext_list.fold_left rest init (fun acc (hash,name) -> 
              Lambda.Lprim 
                (Psequor , 
                 [acc ;
                  Lprim(Pintcomp Ceq, 
                        [arg; 
                         Lconst ((Const_pointer (hash, Pt_variant{name})))],
                        Location.none)], Location.none)
            )
        | _ -> assert false      
      in    
      Lifthenelse (predicate,
                   act1, acc
                  ))
    

  | [], None -> assert false

let make_test_sequence_variant_constant 
  (fail : lam option) (arg : lam) 
  (int_lambda_list : (int * (string * lam) ) list) : lam=
  match int_lambda_list, fail with 
  | (_, (_,act)) :: rest, None -> 
    Ext_list.fold_right rest act (fun (hash1,(name,act1)) acc -> 
        Lifthenelse (Lprim(Pintcomp Ceq, 
          [arg; Lconst ((Const_pointer (hash1, Pt_variant{name})))], Location.none),
          act1, acc
          )
      )
  | _, Some fail -> 
    Ext_list.fold_right int_lambda_list fail (fun (hash1,(name,act1)) acc -> 
        Lifthenelse (Lprim(Pintcomp Ceq, 
                           [arg; Lconst (Const_pointer(hash1, Pt_variant{name}))], Location.none),
                     act1, acc
                    )
      )
  | [], None -> assert false    

let call_switcher_variant_constant 
    (_loc : Location.t) 
    (fail : lam option) 
    (arg : lam) 
    (int_lambda_list :  (int * (string * lam)) list) 
    (_names : Lambda.switch_names option) =
  Ext_log.dwarn ~__POS__ "%a@." Ext_obj.pp_any _names;
  match int_lambda_list, fail with 
  | (_, (_,act)) :: rest, None -> 
    Ext_list.fold_right rest act (fun (hash1,(name,act1)) acc -> 
        Lifthenelse (Lprim(Pintcomp Ceq, 
                           [arg; Lconst (Const_pointer(hash1, Pt_variant{name}))], Location.none),
                     act1, acc
                    )
      )
  | _, Some fail -> 
    Ext_list.fold_right int_lambda_list fail (fun (hash1,(name,act1)) acc -> 
        Lifthenelse (Lprim(Pintcomp Ceq, 
                           [arg; Lconst (Const_pointer(hash1, Pt_variant{name}))], Location.none),
                     act1, acc
                    )
      )
  | [], None -> assert false    


let call_switcher_variant_constr 
    (loc : Location.t) 
    (fail : lam option) 
    (arg  : lam) 
    int_lambda_list 
    (names : Lambda.switch_names option) : lam =
  let v = Ident.create "variant" in
  Llet(Alias, Pgenval, v, Lprim(Pfield (0, Fld_poly_var_tag), [arg], loc),
       call_switcher_variant_constant
         loc fail (Lvar v) int_lambda_list names)