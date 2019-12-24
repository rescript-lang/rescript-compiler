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

let process_getter_setter 
    ~not_getter_setter
    ~(get : Parsetree.core_type -> _ -> Parsetree.attributes -> _) ~set
    loc name
    (attrs : Ast_attributes.t)
    (ty : Parsetree.core_type) (acc : _ list)  =
  match Ast_attributes.process_method_attributes_rev attrs with 
  | {get = None; set = None}, _  ->  not_getter_setter ty :: acc 
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
      set ty 
    ({name with txt = name.Asttypes.txt ^ Literals.setter_suffix} : _ Asttypes.loc)
      pctf_attributes         
      :: get_acc 


let handle_class_type_field self
    ({pctf_loc = loc } as ctf : Parsetree.class_type_field)
    acc =
  match ctf.pctf_desc with 
  | Pctf_method 
      (name, private_flag, virtual_flag, ty) 
    ->
    let not_getter_setter (ty : Parsetree.core_type) =
      let ty = 
        match ty.ptyp_desc with 
        | Ptyp_arrow (label, args, body) 
          ->
          Ast_util.to_method_type
            ty.ptyp_loc  self label args body

        | Ptyp_poly (strs, {ptyp_desc = Ptyp_arrow (label, args, body);
                            ptyp_loc})
          ->
          {ty with ptyp_desc = 
                     Ptyp_poly(strs,             
                               Ast_util.to_method_type
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
                      Ast_util.to_method_type
                        loc self Ast_compatible.no_label ty
                        (Ast_literal.type_unit ~loc ())
                     );
       pctf_attributes} in
    process_getter_setter ~not_getter_setter ~get ~set loc name ctf.pctf_attributes ty acc     

  | Pctf_inherit _ 
  | Pctf_val _ 
  | Pctf_constraint _
  | Pctf_attribute _ 
  | Pctf_extension _  -> 
    Bs_ast_mapper.default_mapper.class_type_field self ctf :: acc 
      

let default_typ_mapper = Bs_ast_mapper.default_mapper.typ
(*
  Attributes are very hard to attribute
  (since ptyp_attributes could happen in so many places), 
  and write ppx extensions correctly, 
  we can only use it locally
*)

let typ_mapper 
    record_as_js_object
    (self : Bs_ast_mapper.mapper)
    (ty : Parsetree.core_type) 
  = 
  match ty with
  | {ptyp_desc = Ptyp_extension({txt = ("bs.obj"|"obj")}, PTyp ty)}
    -> 
    Ext_ref.non_exn_protect record_as_js_object true 
      (fun _ -> self.typ self ty )
  | {ptyp_attributes ;
     ptyp_desc = Ptyp_arrow (label, args, body);
     (* let it go without regard label names, 
        it will report error later when the label is not empty
     *)     
     ptyp_loc = loc
    } ->
    begin match  Ast_attributes.process_attributes_rev ptyp_attributes with 
      | Uncurry _, ptyp_attributes ->
        Ast_util.to_uncurry_type loc self label args body 
      | Meth_callback _, ptyp_attributes ->
        Ast_util.to_method_callback_type loc self label args body
      | Method _, ptyp_attributes ->
        Ast_util.to_method_type loc self label args body
      | Nothing , _ -> 
        Bs_ast_mapper.default_mapper.typ self ty
    end
  | {
    ptyp_desc =  Ptyp_object ( methods, closed_flag) ;
    ptyp_loc = loc 
  } -> 
    let (+>) attr (typ : Parsetree.core_type) =
      {typ with ptyp_attributes = attr :: typ.ptyp_attributes} in           
    let new_methods =
      Ext_list.fold_right  methods []  (fun  meth_ acc ->
        match meth_ with 
        | Parsetree.Oinherit _ -> meth_ :: acc 
        | Parsetree.Otag 
          (label, ptyp_attrs, core_type) -> 
          let get ty name attrs =
            let attrs, core_type =
              match Ast_attributes.process_attributes_rev attrs with
              | Nothing, attrs -> attrs, ty (* #1678 *)
              | Uncurry attr , attrs ->
                attrs, attr +> ty
              | Method _, _
                -> Location.raise_errorf ~loc "bs.get/set conflicts with bs.meth"
              | Meth_callback attr, attrs ->
                attrs, attr +> ty 
            in 
            Ast_compatible.object_field name  attrs (self.typ self core_type) in
          let set ty name attrs =
            let attrs, core_type =
              match Ast_attributes.process_attributes_rev attrs with
              | Nothing, attrs -> attrs, ty
              | Uncurry attr, attrs ->
                attrs, attr +> ty 
              | Method _, _
                -> Location.raise_errorf ~loc "bs.get/set conflicts with bs.meth"
              | Meth_callback attr, attrs ->
                attrs, attr +> ty
            in               
            Ast_compatible.object_field name attrs (Ast_util.to_method_type loc self Ast_compatible.no_label core_type 
              (Ast_literal.type_unit ~loc ())) in
          let not_getter_setter ty =
            let attrs, core_type =
              match Ast_attributes.process_attributes_rev ptyp_attrs with
              | Nothing, attrs -> attrs, ty
              | Uncurry attr, attrs ->
                attrs, attr +> ty 
              | Method attr, attrs -> 
                attrs, attr +> ty 
              | Meth_callback attr, attrs ->
                attrs, attr +> ty  in            
            Ast_compatible.object_field label attrs (self.typ self core_type) in
          process_getter_setter ~not_getter_setter ~get ~set
            loc label ptyp_attrs core_type acc
        )in      
    let inner_type =
      { ty
        with ptyp_desc = Ptyp_object(new_methods, closed_flag);
      } in 
    if !record_as_js_object then 
      Ast_comb.to_js_type loc inner_type          
    else inner_type
  | _ -> default_typ_mapper self ty
    
let handle_class_type_fields self fields = 
  Ext_list.fold_right fields []
  (handle_class_type_field self)
  
