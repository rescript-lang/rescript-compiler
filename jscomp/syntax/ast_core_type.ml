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
  | Empty (* it will be ignored , side effect will be recorded *)



let extract_option_type_exn (ty : t) =
  begin match ty with
    | {ptyp_desc =
         Ptyp_constr
           ({txt =
               Ldot (Lident "*predef*", "option")
               | Lident "option"
            },
            [ty])}
      ->
      ty
    | _ -> assert false
  end

let extract_option_type (ty : t) =
  match ty.ptyp_desc with
  | Ptyp_constr(
    {txt = (Ldot (Lident "*predef*", "option")
      | Lident "option")},
     [ty]) -> Some ty
  | _ -> None

let predef_option : Longident.t =
  Longident.Ldot (Lident "*predef*", "option")

let predef_int : Longident.t =
  Ldot (Lident "*predef*", "int")


let lift_option_type ({ptyp_loc} as ty:t) : t =
  {ptyp_desc =
     Ptyp_constr(
       {txt = predef_option;
        loc = ptyp_loc}
        , [ty]);
        ptyp_loc = ptyp_loc;
      ptyp_attributes = []
    }

let is_any (ty : t) =
  ty.ptyp_desc = Ptyp_any

open Ast_helper

let replace_result (ty : t) (result : t) : t =
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

let is_user_option (ty : t) =
  match ty.ptyp_desc with
  | Ptyp_constr(
    {txt = Lident "option" |
     (Ldot (Lident "*predef*", "option")) },
    [_]) -> true
  | _ -> false

let is_user_bool (ty : t) =
  match ty.ptyp_desc with
  | Ptyp_constr({txt = Lident "bool"},[]) -> true
  | _ -> false

let is_user_int (ty : t) =
  match ty.ptyp_desc with
  | Ptyp_constr({txt = Lident "int"},[]) -> true
  | _ -> false

let is_optional_label l =
  String.length l > 0 && l.[0] = '?'

let label_name l : arg_label =
  if l = "" then Empty else
  if is_optional_label l
  then Optional (String.sub l 1 (String.length l - 1))
  else Label l


(* Note that OCaml type checker will not allow arbitrary
   name as type variables, for example:
   {[
     '_x'_
   ]}
   will be recognized as a invalid program
*)
let from_labels ~loc arity labels
  : t =
  let tyvars =
    ((Ext_list.init arity (fun i ->
         Typ.var ~loc ("a" ^ string_of_int i)))) in
  let result_type =
    Ast_comb.to_js_type loc
      (Typ.object_ ~loc
         (Ext_list.map2 (fun x y -> x.Asttypes.txt ,[], y) labels tyvars) Closed)
  in
  Ext_list.fold_right2
    (fun {Asttypes.loc ; txt = label }
      tyvar acc -> Typ.arrow ~loc label tyvar acc) labels tyvars  result_type


let make_obj ~loc xs =
  Ast_comb.to_js_type loc
    (Ast_helper.Typ.object_  ~loc xs Closed)


let opt_arrow loc label ty1 ty2 =
  Typ.arrow ~loc ("?" ^ label) ty1 ty2
(**

{[ 'a . 'a -> 'b ]}
OCaml does not support such syntax yet
{[ 'a -> ('a. 'a -> 'b) ]}

*)
let rec get_uncurry_arity_aux  (ty : t) acc =
    match ty.ptyp_desc with
    | Ptyp_arrow(_, _ , new_ty) ->
      get_uncurry_arity_aux new_ty (succ acc)
    | Ptyp_poly (_,ty) ->
      get_uncurry_arity_aux ty acc
    | _ -> acc

(**
   {[ unit -> 'a1 -> a2']}  arity 2
   {[ unit -> 'b ]} return arity 0
   {[ 'a1 -> 'a2 -> ... 'aN -> 'b ]} return arity N
*)
let get_uncurry_arity (ty : t ) =
  match ty.ptyp_desc  with
  | Ptyp_arrow("", {ptyp_desc = (Ptyp_constr ({txt = Lident "unit"}, []))},
    ({ptyp_desc = Ptyp_arrow _ } as rest  )) -> `Arity (get_uncurry_arity_aux rest 1 )
  | Ptyp_arrow("", {ptyp_desc = (Ptyp_constr ({txt = Lident "unit"}, []))}, _) -> `Arity 0
  | Ptyp_arrow(_,_,rest ) ->
    `Arity(get_uncurry_arity_aux rest 1)
  | _ -> `Not_function

let get_curry_arity  ty =
  get_uncurry_arity_aux ty 0

let is_arity_one ty = get_curry_arity ty =  1

let list_of_arrow (ty : t) =
  let rec aux (ty : t) acc =
    match ty.ptyp_desc with
    | Ptyp_arrow(label,t1,t2) ->
      aux t2 ((label,t1,ty.ptyp_attributes,ty.ptyp_loc) ::acc)
    | Ptyp_poly(_, ty) -> (* should not happen? *)
      Bs_syntaxerr.err ty.ptyp_loc Unhandled_poly_type
    | return_type -> ty, List.rev acc
  in aux ty []
