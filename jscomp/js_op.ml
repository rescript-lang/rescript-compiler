(* OCamlScript compiler
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



(** Define some basic types used in JS IR *)

type binop =
  | Eq  
  (* acutally assignment ..
     TODO: move it into statement, so that all expressions 
     are side efffect free (except function calls)
   *)

  | Or
  | And
  | EqEqEq
  | NotEqEq

  | Lt
  | Le
  | Gt
  | Ge

  | Bor
  | Bxor
  | Band
  | Lsl
  | Lsr
  | Asr

  | Plus
  | Minus
  | Mul
  | Div
  | Mod

type int_op = 
    
  | Bor
  | Bxor
  | Band
  | Lsl
  | Lsr
  | Asr

  | Plus
  | Minus
  | Mul
  | Div
  | Mod

(* https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Expressions_and_Operators#Bitwise_operators
    {[
    ~
    ]}
    ~0xff -> -256
    design; make sure each operation type is consistent
 *)
type level = 
  | Log 
  | Info
  | Warn
  | Error

type kind = 
  | Ml
  | Runtime 
  | External of string

type property = 
  | Mutable
  | Immutable

type int_or_char = 
    { i :  int;
      c : char option
    }
 (* literal char *)

type number = 
  | Float of float 
  | Int of int_or_char

type mutable_flag = 
  | Mutable
  | Immutable
  | NA

(* 
    {[
    let rec x = 1 :: y 
    and y = 1 :: x
    ]}
 *)
type recursive_info = 
  | SingleRecursive 
  | NonRecursie
  | NA

type used_stats = 
  | Dead_pure 
        (* only [Dead] should be taken serious, 
            other status can be converted during
            inlining
            -- all exported symbols can not be dead
            -- once a symbole is called Dead_pure, 
            it can not be alive anymore, we should avoid iterating it
            
          *)
  | Dead_non_pure 
      (* we still need iterating it, 
         just its bindings does not make sense any more *)
  | Exported (* Once it's exported, shall we change its status anymore? *)
      (* In general, we should count in one pass, and eliminate code in another 
         pass, you can not do it in a single pass, however, some simple 
         dead code can be detected in a single pass
       *)
  | Used
  | Scanning_pure
  | Scanning_non_pure
  | NA


type ident_info = {
    (* mutable recursive_info : recursive_info; *)
    mutable used_stats : used_stats;
  }

type exports = Ident.t list 

type required_modules = (Ident.t * string) list
(** TODO: define constant - for better constant folding  *)
(* type constant =  *)
(*   | Const_int of int *)
(*   | Const_ *)
