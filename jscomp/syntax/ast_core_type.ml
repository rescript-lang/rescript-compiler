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

type t = Parsetree.core_type 
type arg_label =
  | Label of string 
  | Optional of string 
  | Empty

type arg_type = 
  | NullString of (int * string) list 
  | NonNullString of (int * string) list 
  | Int of (int * int ) list 
  | Array 
  | Unit
  | Nothing
  | Ignore

open Ast_helper
(** TODO check the polymorphic *)
let list_of_arrow (ty : t) = 
  let rec aux (ty : Parsetree.core_type) acc = 
    match ty.ptyp_desc with 
    | Ptyp_arrow(label,t1,t2) -> 
      aux t2 ((label,t1) ::acc)
    | Ptyp_poly(_, ty) -> (* should not happen? *)
      aux ty acc 
    | return_type -> ty, List.rev acc
  in aux ty []

let replace_result ty result = 
  let rec aux (ty : Parsetree.core_type) = 
    match ty with 
    | { ptyp_desc = 
          Ptyp_arrow (label,t1,t2)
      } -> { ty with ptyp_desc = Ptyp_arrow(label,t1, aux t2)}
    | {ptyp_desc = Ptyp_poly(fs,ty)} 
      ->  {ty with ptyp_desc = Ptyp_poly(fs, aux ty)}
    | _ -> result in 
  aux ty 

let is_unit (ty : t ) = 
  match ty.ptyp_desc with 
  | Ptyp_constr({txt =Lident "unit"}, []) -> true
  | _ -> false 

let is_array (ty : t) = 
  match ty.ptyp_desc with 
  | Ptyp_constr({txt =Lident "array"}, [_]) -> true
  | _ -> false 

let is_optional l =
  String.length l > 0 && l.[0] = '?'

let label_name l : arg_label =
  if l = "" then Empty else 
  if is_optional l 
  then Optional (String.sub l 1 (String.length l - 1))
  else Label l


let from_labels ~loc tyvars (labels : string list)
  : t = 
  let result_type =
    Ast_comb.to_js_type loc  
     (Typ.object_ ~loc (List.map2 (fun x y -> x ,[], y) labels tyvars) Closed)
  in 
  List.fold_right2 
    (fun label tyvar acc -> Typ.arrow ~loc label tyvar acc) labels tyvars  result_type


let make_obj ~loc xs =
  Ast_comb.to_js_type loc @@
  Ast_helper.Typ.object_  ~loc xs   Closed
