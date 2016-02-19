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



type jbl_label = int 

module HandlerMap = Map.Make(struct 
  type t = jbl_label
  let compare x y= compare (x:t) y 
end )

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
  | False 
  | True of return_label option 
   (* have a mutable field to notifiy it's actually triggered *)
   (* anonoymous function does not have identifier *)

type let_kind = Lambda.let_kind

type st = 
  | EffectCall
  | Declare of let_kind * J.ident (* bound value *)
  | NeedValue 
  | Assign of J.ident (* when use [Assign], var is not needed, since it's already declared  *)

type cxt = {
  st : st ;
  should_return : return_type;
  jmp_table : value  HandlerMap.t ;
  meta : Lam_stats.meta ;
  (* include_alias :  *)
  (*   (\** It's correct to add more, we can do this in lambda optimization pass *)
  (*    *\) *)
  (*   (Ident.t , Ident.t) Hashtbl.t *)
  (* Used when compiling [Lstaticraise]  *)
}

let empty_handler_map = HandlerMap.empty


let add_jmps (exit_id, code_table)   
    (m : value HandlerMap.t) = 
  (* always keep key id positive, specifically no [0] generated
   *)
  let map, _, handlers = 
    List.fold_left 
           (fun (acc,prev_order_id, handlers) 
               (l,lam, args)   -> 
                 let order_id = prev_order_id + 1 in
                 (HandlerMap.add l {exit_id ; args; order_id } acc, 
                  order_id ,
                  (order_id, lam) :: handlers))
      (m,
       HandlerMap.cardinal m,
       []
      )
      code_table in
  map, List.rev handlers
