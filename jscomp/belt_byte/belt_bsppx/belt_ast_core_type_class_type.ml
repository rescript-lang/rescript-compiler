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
open Ast_helper
let process_getter_setter ~no ~get ~set
    loc name
    (attrs : Belt_ast_attributes.t)
    (ty : Parsetree.core_type) acc  =
  match Belt_ast_attributes.process_method_attributes_rev attrs with 
  | {get = None; set = None}, _  ->  no ty :: acc 
  | st , pctf_attributes
    -> 
    let get_acc = 
      match st.set with 
      | Some `No_get -> acc 
      | None 
      | Some `Get -> 
        let lift txt = 
          Typ.constr ~loc {txt ; loc} [ty] in
        let (null,undefined) =                
          match st with 
          | {get = Some (null, undefined) } -> (null, undefined)
          | {get = None} -> (false, false ) in 
        let ty = 
          match (null,undefined) with 
          | false, false -> ty
          | true, false -> lift Ast_literal.Lid.js_null
          | false, true -> lift Ast_literal.Lid.js_undefined
          | true , true -> lift Ast_literal.Lid.js_null_undefined in
        get ty name pctf_attributes
        :: acc  
    in 
    if st.set = None then get_acc 
    else
      set ty (name ^ Literals.setter_suffix) pctf_attributes         
      :: get_acc 


(* let handle_class_type_field self
    ({pctf_loc = loc } as ctf : Parsetree.class_type_field)
    acc =
  match ctf.pctf_desc with 
  | Pctf_method 
      (name, private_flag, virtual_flag, ty) 
    ->
    let no (ty : Parsetree.core_type) =
      let ty = 
        match ty.ptyp_desc with 
        | Ptyp_arrow (label, args, body) 
          ->
          Belt_ast_util.to_method_type
            ty.ptyp_loc  self label args body

        | Ptyp_poly (strs, {ptyp_desc = Ptyp_arrow (label, args, body);
                            ptyp_loc})
          ->
          {ty with ptyp_desc = 
                     Ptyp_poly(strs,             
                               Belt_ast_util.to_method_type
                                 ptyp_loc  self label args body  )}
        | _ -> 
          self.typ self ty
      in 
      {ctf with 
       pctf_desc = 
         Pctf_method (name , private_flag, virtual_flag, ty)}
    in
    let get ty name pctf_attributes =
      {ctf with 
       pctf_desc =  
         Pctf_method (name , 
                      private_flag, 
                      virtual_flag, 
                      self.typ self ty
                     );
       pctf_attributes} in
    let set ty name pctf_attributes =
      {ctf with 
       pctf_desc =
         Pctf_method (name, 
                      private_flag,
                      virtual_flag,
                      Belt_ast_util.to_method_type
                        loc self "" ty
                        (Ast_literal.type_unit ~loc ())
                     );
       pctf_attributes} in
    process_getter_setter ~no ~get ~set loc name ctf.pctf_attributes ty acc     

  | Pctf_inherit _ 
  | Pctf_val _ 
  | Pctf_constraint _
  | Pctf_attribute _ 
  | Pctf_extension _  -> 
    Bs_ast_mapper.default_mapper.class_type_field self ctf :: acc  *)
      

(*
  Attributes are very hard to attribute
  (since ptyp_attributes could happen in so many places), 
  and write ppx extensions correctly, 
  we can only use it locally
*)

let handle_core_type 
    ~(super : Bs_ast_mapper.mapper) 
    ~(self : Bs_ast_mapper.mapper)
    (ty : Parsetree.core_type) 
    record_as_js_object
  = 
  match ty with
  (* 
  | {ptyp_desc = Ptyp_extension({txt = ("bs.obj"|"obj")}, PTyp ty)}
    -> 
    Ext_ref.non_exn_protect record_as_js_object true 
      (fun _ -> self.typ self ty ) *)
  | {ptyp_attributes ;
     ptyp_desc = Ptyp_arrow (label, args, body);
     (* let it go without regard label names, 
        it will report error later when the label is not empty
     *)     
     ptyp_loc = loc
    } ->
    begin match  Belt_ast_attributes.process_attributes_rev ptyp_attributes with 
      | `Uncurry , ptyp_attributes ->
        Belt_ast_util.to_uncurry_type loc self label args body 
      | _ ->
      (* |  `Meth_callback, ptyp_attributes ->
        Belt_ast_util.to_method_callback_type loc self label args body
      | `Method, ptyp_attributes ->
        Belt_ast_util.to_method_type loc self label args body
      | `Nothing , _ ->  *)
        Bs_ast_mapper.default_mapper.typ self ty
    end
  (* | {
    ptyp_desc =  Ptyp_object ( methods, closed_flag) ;
    ptyp_loc = loc 
  } -> 
    let (+>) attr (typ : Parsetree.core_type) =
      {typ with ptyp_attributes = attr :: typ.ptyp_attributes} in           
    let new_methods =
      Ext_list.fold_right (fun (label, ptyp_attrs, core_type) acc ->
          let get ty name attrs =
            let attrs, core_type =
              match Belt_ast_attributes.process_attributes_rev attrs with
              | `Nothing, attrs -> attrs, ty (* #1678 *)
              | `Uncurry, attrs ->
                attrs, Belt_ast_attributes.bs +> ty
              | `Method, _
                -> Location.raise_errorf ~loc "bs.get/set conflicts with bs.meth"
              | `Meth_callback, attrs ->
                attrs, Belt_ast_attributes.bs_this +> ty 
            in 
            name , attrs, self.typ self core_type in
          let set ty name attrs =
            let attrs, core_type =
              match Belt_ast_attributes.process_attributes_rev attrs with
              | `Nothing, attrs -> attrs, ty
              | `Uncurry, attrs ->
                attrs, Belt_ast_attributes.bs +> ty 
              | `Method, _
                -> Location.raise_errorf ~loc "bs.get/set conflicts with bs.meth"
              | `Meth_callback, attrs ->
                attrs, Belt_ast_attributes.bs_this +> ty
            in               
            name, attrs, Belt_ast_util.to_method_type loc self "" core_type 
              (Ast_literal.type_unit ~loc ()) in
          let no ty =
            let attrs, core_type =
              match Belt_ast_attributes.process_attributes_rev ptyp_attrs with
              | `Nothing, attrs -> attrs, ty
              | `Uncurry, attrs ->
                attrs, Belt_ast_attributes.bs +> ty 
              | `Method, attrs -> 
                attrs, Belt_ast_attributes.bs_method +> ty 
              | `Meth_callback, attrs ->
                attrs, Belt_ast_attributes.bs_this +> ty  in            
            label, attrs, self.typ self core_type in
          process_getter_setter ~no ~get ~set
            loc label ptyp_attrs core_type acc
        ) methods [] in      
    let inner_type =
      { ty
        with ptyp_desc = Ptyp_object(new_methods, closed_flag);
      } in 
    if !record_as_js_object then 
      Ast_comb.to_js_type loc inner_type          
    else inner_type *)
  | _ -> super.typ self ty
    
(* let handle_class_type_fields self fields = 
  Ext_list.fold_right 
  (handle_class_type_field self)
  fields [] *)
  
let handle_core_type self typ record_as_js_object =
  handle_core_type 
  ~super:Bs_ast_mapper.default_mapper
  ~self typ record_as_js_object
