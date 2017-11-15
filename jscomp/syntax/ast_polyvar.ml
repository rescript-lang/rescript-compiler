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


 let map_row_fields_into_ints ptyp_loc
    (row_fields : Parsetree.row_field list) 
  = 
  let _, acc, rev_row_fields = 
    (List.fold_left 
       (fun (i,acc, row_fields) rtag -> 
          match rtag with 
          | Parsetree.Rtag (label, attrs, true,  [])
            -> 
            begin match Ast_attributes.process_bs_int_as attrs with 
              | Some i, new_attrs -> 
                i + 1, ((Ext_pervasives.hash_variant label , i):: acc ), 
                Parsetree.Rtag (label, new_attrs, true, []) :: row_fields
              | None, _ -> 
                i + 1 , ((Ext_pervasives.hash_variant label , i):: acc ), rtag::row_fields
            end

          | _ -> 
            Bs_syntaxerr.err ptyp_loc Invalid_bs_int_type

       ) (0, [],[]) row_fields) in 
  List.rev acc, List.rev rev_row_fields              

(** It also check in-consistency of cases like 
    {[ [`a  | `c of int ] ]}       
*)  
let map_row_fields_into_strings ptyp_loc 
    (row_fields : Parsetree.row_field list) = 
  let case, result, row_fields  = 
    (Ext_list.fold_right (fun tag (nullary, acc, row_fields) -> 
         match nullary, tag with 
         | (`Nothing | `Null), 
           Parsetree.Rtag (label, attrs, true,  [])
           -> 
           begin match Ast_attributes.process_bs_string_as attrs with 
             | Some name, new_attrs  -> 
               `Null, ((Ext_pervasives.hash_variant label, name) :: acc ), 
               Parsetree.Rtag(label, new_attrs, true, []) :: row_fields

             | None, _ -> 
               `Null, ((Ext_pervasives.hash_variant label, label) :: acc ), 
               tag :: row_fields
           end
         | (`Nothing | `NonNull), Parsetree.Rtag(label, attrs, false, ([ _ ] as vs)) 
           -> 
           begin match Ast_attributes.process_bs_string_as attrs with 
             | Some name, new_attrs -> 
               `NonNull, ((Ext_pervasives.hash_variant label, name) :: acc),
               Parsetree.Rtag (label, new_attrs, false, vs) :: row_fields
             | None, _ -> 
               `NonNull, ((Ext_pervasives.hash_variant label, label) :: acc),
               (tag :: row_fields)
           end
         | _ -> Bs_syntaxerr.err ptyp_loc Invalid_bs_string_type

       ) row_fields (`Nothing, [], [])) in 
  (match case with 
   | `Nothing -> Bs_syntaxerr.err ptyp_loc Invalid_bs_string_type
   | `Null -> External_arg_spec.NullString result 
   | `NonNull -> NonNullString result), row_fields

  
  let is_enum row_fields = 
    List.for_all (fun (x : Parsetree.row_field) -> 
      match x with 
      | Rtag(_label,_attrs,true, []) -> true 
      | _ -> false
    ) row_fields
