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

type t = Parsetree.payload

let is_single_string (x : t ) = 
  match x with  (** TODO also need detect empty phrase case *)
  | PStr [ {
      pstr_desc =  
        Pstr_eval (
          {pexp_desc = 
             Pexp_constant 
               (Const_string (name,_));
           _},_);
      _}] -> Some name
  | _  -> None


let is_string_or_strings (x : t) : 
  [ `None | `Single of string | `Some of string list ] = 
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
  | PStr [ {
      pstr_desc =  
        Pstr_eval (
          {pexp_desc = 
             Pexp_constant 
               (Const_string (name,_));
           _},_);
      _}] -> `Single name 
  | _ -> `None
