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








let is_single_string (x : Parsetree.payload ) = 
  match x with  (** TODO also need detect empty phrase case *)
  | Parsetree.PStr [ {
      pstr_desc =  
        Pstr_eval (
          {pexp_desc = 
             Pexp_constant 
               (Const_string (name,_));
           _},_);
      _}] -> Some name
  | _  -> None

let is_string_or_strings (x : Parsetree.payload ) :  [ `None | `Single of string | `Some of string list ] = 
  let module M = struct exception Not_str end  in 
  match x with 
  | PStr [ {pstr_desc =  
              Pstr_eval (
                {pexp_desc = 
                   Pexp_apply
                     ({pexp_desc = Pexp_constant (Const_string (name,_)); _},
                      args
                     );
                 _},_);
            _}] ->
    (try 
       `Some (name :: (args |> List.map (fun (_label,e) ->
           match (e : Parsetree.expression) with
           | {pexp_desc = Pexp_constant (Const_string (name,_)); _} -> 
             name
           | _ -> raise M.Not_str)))

     with M.Not_str -> `None )
  |  Parsetree.PStr [ {
      pstr_desc =  
        Pstr_eval (
          {pexp_desc = 
             Pexp_constant 
               (Const_string (name,_));
           _},_);
      _}] -> `Single name 
  | _ -> `None

let lift_int ?loc ?attrs x = 
  Ast_helper.Exp.constant ?loc ?attrs (Const_int x)

let has_arity (attrs : Parsetree.attributes) = 
  Ext_list.find_opt (fun (attr : Parsetree.attribute)  -> 
      match attr with 
      | {txt = "arity"; _ }, 
        PStr [ { pstr_desc = Pstr_eval 
                     ( {pexp_desc = Pexp_constant (Const_int i)},_attr);
                 _}]
        -> 
        if i >= 0 then 
          Some i
        else None
      | _ -> None 
    ) attrs  



let arity_from_core_type (x : Parsetree.core_type) = 
  let rec aux acc (x : Parsetree.core_type) = 
    match x.ptyp_desc with 
    | Ptyp_arrow (_,_,r) -> 
      (* 'a -> ('b -> ('c -> 'd )) *)
      aux (acc + 1) r 
    | _ -> acc  in 
  aux 0 x



let attr_attribute_from_type (x : Parsetree.core_type) : Parsetree.attribute = 
  let n = arity_from_core_type x in 
  let loc = x.ptyp_loc in
  {txt = "arity"; loc},
  PStr ([ {pstr_desc = 
             Pstr_eval (lift_int n,[]);
           pstr_loc = loc
          }])
