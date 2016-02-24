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



(**

Note that js object support gettter, setter

{[ 
  var obj = {
    get latest () {
    if (log.length == 0) return undefined;
    return log[log.length - 1]
    }
  } 
]}

If it's getter then the [.length] property does not make sense, 
we should avoid using it which means when branch to js, if there is no 
args, we should not do 
{{ Curry.app0(obj.name) }} 
*)
type t = 
  | Js_index
  (* index__js
     {[ x[i] ]}
  *)
  | Js_set_index 
  (* set_index__js 
     {[ x[i]= 3 ]}     
  *)
  | Js_set
  (* __set
     {[ x.h = 3 ]}
  *)


  | Js of int option
  | Ml of int option
  | Unknown of int option 

let process ( x : string) : t * string =

  match x with
  | "index__unsafe_js"
  (* TODO: [unsafe] should be allowed to overloaded in general  *)
  | "index__js_unsafe"
  | "index__unsafe" 
  | "index__" 
    -> (Js_index, "index")
  | "index__set_unsafe" 
  | "index__set" 
  | "index__set_js_unsafe" 
  | "index__set_js" 
    -> (Js_set_index, "index")
  | _ -> 
    let sub = "__" in
    let v = Ext_string.rfind ~sub x  in
    if  v < 0 then 
      (Unknown None  , x)
    else 
      let len_sub = String.length sub in
      let indicator = Ext_string.tail_from x (v + len_sub) in 
      let normal_name  = String.sub x 0 v in 
      match indicator with 
      | "set" -> Js_set, normal_name

      | _ -> 
        let props = Ext_string.split indicator '_' in 
        let kind = ref None in 
        let arity = ref None in 
        let fail l = 
          let error = "invalid indicator" ^ indicator  ^ "in method name " ^ 
                      x ^ ":" ^ Lam_current_unit.get_file () in
          Ext_log.err l "%s" error ; 
          failwith error in
        let update_ref r k = 
          match !r with 
          | None -> r := Some k 
          | Some x -> if x != k then fail __LOC__ in
        List.iter (fun  x -> 
          match x with 
          | "js" 
            -> update_ref kind `Js
          | "ml"
            -> update_ref kind  `Ml
          | "gen" 
            -> update_ref kind `Unknown
          | _ -> 
            match int_of_string x with 
            | exception _ -> fail __LOC__
            | v -> 
              update_ref arity v
        ) props ;
      (
        let arity  = !arity in
      begin match  !kind with 
      | Some  `Js 
      | None 
        -> Js arity
      | Some `Ml -> Ml arity
      | Some `Unknown -> Unknown arity

      end, normal_name
      )

