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

 type loc = Location.t 

 type acc =  
  (Asttypes.override_flag * Longident.t Asttypes.loc * loc *
      Parsetree.attributes)  list

let rec is_simple_pattern (p : Parsetree.pattern) =   
  match p.ppat_desc with 
  | Ppat_any -> true 
  | Ppat_var _ -> true 
  | Ppat_constraint(p,_) -> is_simple_pattern p  
  | _ -> false
    
(** 
  destruct such pattern 
  {[ A.B.let open C in (a,b)]}
*)        
let rec destruct_open     
    (e : Parsetree.expression) (acc : acc) 
    : (acc * Parsetree.expression list) option = 
  match e.pexp_desc with 
  | Pexp_open (flag, lid, cont)
    -> 
    destruct_open 
      cont 
      ((flag, lid, e.pexp_loc, e.pexp_attributes) :: acc)
  | Pexp_tuple es -> Some (acc, es)
  | _ -> None


(*
  [let (a,b) = M.N.(c,d) ]
  => 
  [ let a = M.N.c 
    and b = M.N.d ]
*)  
let flattern_tuple_pattern_vb 
    (self : Bs_ast_mapper.mapper)   
    ({pvb_loc } as vb :  Parsetree.value_binding)
    acc : Parsetree.value_binding list =
  let pvb_pat = self.pat self vb.pvb_pat in 
  let pvb_expr = self.expr self vb.pvb_expr in  
  let pvb_attributes = self.attributes self vb.pvb_attributes in 
  match destruct_open pvb_expr [] , pvb_pat.ppat_desc with 
  | Some (wholes, es), Ppat_tuple xs 
    when 
      List.for_all is_simple_pattern xs &&
      Ext_list.same_length es xs 
    -> 
    (Ext_list.fold_right2 (fun pat exp acc-> 
         {Parsetree.
           pvb_pat = 
             pat;
           pvb_expr =  
             ( match wholes with 
               | [] -> exp  
               | _ ->
                 List.fold_left (fun x (flag,lid,loc,attrs)  ->
                     {Parsetree.
                       pexp_desc = Pexp_open(flag,lid,x); 
                       pexp_attributes = attrs;
                       pexp_loc = loc
                     }
                   ) exp wholes) ;
           pvb_attributes; 
           pvb_loc ;
         } :: acc 
       ) xs es) acc
  | _ -> 
    {pvb_pat ; 
     pvb_expr ;
     pvb_loc ;
     pvb_attributes} :: acc 



let handle_value_bindings  =
  fun self (vbs : Parsetree.value_binding list) -> 
     (* Bs_ast_mapper.default_mapper.value_bindings self  vbs   *)
     List.fold_right (fun vb acc ->
         flattern_tuple_pattern_vb self vb acc 
       ) vbs []     