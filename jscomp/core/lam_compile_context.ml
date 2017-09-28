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




type jbl_label = int 

module HandlerMap = Int_map
type value = {
  exit_id : Ident.t ;
  args : Ident.t list ;
  order_id : int
}

(* delegate to the callee to generate expression 
      Invariant: [output] should return a trailing expression
*)
type return_label = {
  id : Ident.t;
  label : J.label;
  params : Ident.t list;
  immutable_mask : bool array; 
  mutable new_params : Ident.t Ident_map.t;  
  mutable triggered : bool
}

type return_type = 
  | ReturnFalse 
  | ReturnTrue of return_label option 
  (* Note [return] does indicate it is a tail position in most cases
     however, in an exception handler, return may not be in tail position
     to fix #1701 we play a trick that (ReturnTrue None) 
     would never trigger tailcall, however, it preserves [return] 
     semantics
  *)
(* have a mutable field to notifiy it's actually triggered *)
(* anonoymous function does not have identifier *)

type let_kind = Lam.let_kind

type cont = 
  | EffectCall
  | Declare of let_kind * J.ident (* bound value *)
  | NeedValue 
  | Assign of J.ident (* when use [Assign], var is not needed, since it's already declared  *)

type jmp_table =   value  HandlerMap.t

type t = {
  st : cont ;
  should_return : return_type;
  jmp_table : jmp_table;
  meta : Lam_stats.t ;
}

let empty_handler_map = HandlerMap.empty



(* always keep key id positive, specifically no [0] generated *)
let add_jmps 
    exit_id code_table
    m = 
  let map, handlers = 
    Ext_list.fold_left_with_offset
      (fun order_id (acc,handlers)
        (l,lam,args)
        ->     
          HandlerMap.add l {exit_id;args; order_id } acc, 
          (order_id,lam)::handlers
      ) (HandlerMap.cardinal m + 1 )  (m,[]) code_table in 
  map, List.rev handlers


let find_exn i cxt = 
  Int_map.find_exn i cxt.jmp_table  