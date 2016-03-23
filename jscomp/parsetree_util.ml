(* BuckleScript compiler
 * Copyright (C) 2015-2016 Bloomberg Finance L.P.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

(* Author: Hongbo Zhang  *)



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


let has_arity (attrs : Parsetree.attributes) = 
  Ext_list.find_opt (fun (attr : Parsetree.attribute)  -> 
      match attr with 
      | {txt = "arity"; _ }, 
        PStr [ { pstr_desc = Pstr_eval ( {pexp_desc = Pexp_constant (Const_int i)},_attr); _}]
        -> Some i
      | _ -> None 
    ) attrs  


let attr_attribute_from_type (x : Parsetree.core_type) : Parsetree.attribute = 
  let rec aux acc (x : Parsetree.core_type) = 
    match x.ptyp_desc with 
    | Ptyp_arrow (_,_,r) -> 
      (* 'a -> ('b -> ('c -> 'd )) *)
      aux (acc + 1) r 
    | _ -> acc  in 
  let n  = aux 0 x  in 
  let loc = x.ptyp_loc in
  {txt = "arity"; loc},
  PStr ([ {pstr_desc = 
             Pstr_eval ({pexp_desc = Pexp_constant (Const_int n); pexp_loc = loc; pexp_attributes = []},[]);
           pstr_loc = loc
          }])
