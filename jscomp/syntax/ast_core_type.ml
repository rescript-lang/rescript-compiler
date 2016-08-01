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

let label_name l =
  if l = "" then `Empty else 
  if is_optional l 
  then `Optional (String.sub l 1 (String.length l - 1))
  else `Label l

let string_type (ty : t) = 
  match ty with 
  | {ptyp_desc; ptyp_attributes; ptyp_loc = loc} -> 
    match Ast_attributes.process_bs_string_int ptyp_attributes with 
    | `String  -> 
      begin match ptyp_desc with 
      | Ptyp_variant ( row_fields, Closed, None)
        -> 
        let case, result = 
          (List.fold_right (fun tag (nullary, acc) -> 
               match nullary, tag with 
               | (`Nothing | `Null), Parsetree.Rtag (label, attrs, true,  [])
                 -> 
                 let name = 
                   match Ast_attributes.process_bs_string_as attrs with 
                   | Some name -> name 
                   | None -> label in
                 `Null, ((Btype.hash_variant label, name) :: acc )
               | (`Nothing | `NonNull), Parsetree.Rtag(label, attrs, false, [ _ ]) 
                 -> 
                 let name = 
                   match Ast_attributes.process_bs_string_as attrs with 
                   | Some name -> name 
                   | None -> label in
                 `NonNull, ((Btype.hash_variant label, name) :: acc)

               | _ -> Location.raise_errorf ~loc "Not a valid string type"
             ) row_fields (`Nothing, [])) in 
        begin match case with 
        | `Nothing -> Location.raise_errorf ~loc "Not a valid string type"
        | `Null -> `NullString result 
        | `NonNull -> `NonNullString result 
        end
      | _ -> Location.raise_errorf ~loc "Not a valid string type"
      end
    | `Int  -> 
      begin match ptyp_desc with 
      | Ptyp_variant ( row_fields, Closed, None)
        -> 
        let _, acc = 
          (List.fold_left 
             (fun (i,acc) rtag -> 
                match rtag with 
                | Parsetree.Rtag (label, attrs, true,  [])
                  -> 
                  let name = 
                    match Ast_attributes.process_bs_int_as attrs with 
                    | Some name -> name 
                    | None -> i in
                  name + 1, ((Btype.hash_variant label , name):: acc )
                | _ -> Location.raise_errorf ~loc "Not a valid string type"
             ) (0, []) row_fields) in 
        `Int (List.rev acc)
          
      | _ -> Location.raise_errorf ~loc "Not a valid string type"
      end

    | `Nothing -> `Nothing
      


let from_labels ~loc tyvars (labels : string list)
  : t = 
  let result_type =
    Ast_comb.to_js_type loc  
     (Typ.object_ ~loc (List.map2 (fun x y -> x ,[], y) labels tyvars) Closed)
  in 
  List.fold_right2 
    (fun label tyvar acc -> Typ.arrow ~loc label tyvar acc) labels tyvars  result_type


type arg_label =
  [ `Label of string | `Optional of string | `Empty]


