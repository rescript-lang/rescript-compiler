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
  | Js_read_index
  (* index__js
     {[ x[i] ]}
  *)
  | Js_write_index 
  (* set_index__js 
     {[ x[i]= 3 ]}     
  *)
  | Js_write
  (* _set
     {[ x.h = 3 ]}
  *)
  | Js_read

  | Js of int option
  | Ml of int option
  | Unknown of int option 

let process ( x : string) : t * string =

  match x with
  | "index__unsafe_r"
  (* TODO: [unsafe] should be allowed to overloaded in general  *)
  | "index__r_unsafe"
  | "index__r" 
  | "index__" 
    -> (Js_read_index, "index")
  | "index__w_unsafe" 
  | "index__w" 
  | "index__w_js_unsafe" 
  | "index__w_js" 
    -> (Js_write_index, "index")
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
      | "r" -> Js_read, normal_name
      | "w" -> Js_write, normal_name
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
          | Some x -> if x <> k then fail __LOC__ in
        List.iter (fun  x -> 
          match x with 
          | "js" 
            -> update_ref kind `Js
          | "ml"
            -> update_ref kind  `Ml
          | "gen" 
            -> update_ref kind `Unknown
          | "unsafe"
            -> 
            (* allow unsafe to be overloaded, but don't do anything yet *)
            ()
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

