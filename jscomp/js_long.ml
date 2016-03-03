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

module E = Js_exp_make

let make_const ~lo ~hi = 
   E.make_block 
     ~comment:"int64" (E.zero_int_literal) 
     Record [E.int lo ; E.int hi] Immutable

let make ~lo ~hi = 
   E.make_block 
     ~comment:"int64" (E.zero_int_literal) 
     Record [ lo ;  hi]
     Immutable
let get_lo x = E.index x 0l
let get_hi x = E.index x 1l

let of_const (v : Int64.t) = 
  make_const
    ~lo:(Int64.to_int32 v )
    ~hi:(Int64.to_int32 (Int64.shift_right v 32))

let to_int32 args = 
  begin match args with
  | [v] ->  E.index v 0l
  | _ -> assert false
  end
let of_int32 (args : J.expression list) = 
  match args with 
  | [{expression_desc = Number (Int {i}) ; _}] 
    -> 
    if i < 0l then make_const ~lo:i ~hi:(-1l)
    else make_const ~lo:i ~hi:0l
  | _ -> E.runtime_call Js_config.int64 "of_int32" args

let comp (cmp : Lambda.comparison) args = 
  E.runtime_call  Js_config.int64
    (match cmp with 
     | Ceq -> "eq"
     | Cneq -> "neq"
     | Clt -> "lt"
     | Cgt -> "gt"
     | Cle -> "le"
     | Cge -> "ge") args 

let neg args = 
  E.runtime_call Js_config.int64 "neg" args

let add args = 
  E.runtime_call Js_config.int64 "add" args

let sub args = 
  E.runtime_call Js_config.int64 "sub" args

let mul args =  
  E.runtime_call Js_config.int64 "mul" args

let div args =
  E.runtime_call Js_config.int64 "div" args

let bit_op  op args = 
  match args  with 
  | [l;r] -> 
    make ~lo:(op (get_lo l) (get_lo r))
      ~hi:(op (get_hi l) (get_hi r))
  | _ -> assert false

let xor  = bit_op E.int32_bxor 
let or_ = bit_op E.int32_bor
let and_ = bit_op E.int32_band


let lsl_ args = 
  E.runtime_call Js_config.int64 "lsl_" args

let lsr_ args = 
  E.runtime_call Js_config.int64 "lsr_" args

let asr_ args = 
  E.runtime_call Js_config.int64 "asr_" args

(*FIXME: todo *)
let mod_ args = 
  E.runtime_call Js_config.int64 "mod_" args


let swap args = 
  E.runtime_call Js_config.int64 "swap" args
