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

let rec translate (x : Lambda.structured_constant ) : J.expression = 
  match x with 
  | Const_base c -> 
    begin match c with 
      | Const_int i -> E.int (Int32.of_int i)
      | Const_char i ->
        Js_of_lam_string.const_char i
      | Const_int32 i -> E.int i 
          (* E.float (Int32.to_string i) *)
      | Const_int64 i -> 
          (*
            TODO:
            {[
            Int64.to_string 0x7FFFFFFFFFFFFFFFL;;
            - : string = "9223372036854775807"
            ]}
            {[
            Int64.(to_float max_int);;
            - : float = 9.22337203685477581e+18
            ]}
            Note we should compile it to Int64 as JS's 
            speical representation -- 
            it is not representatble in JS number
           *)
          E.float (Int64.to_string i)
      | Const_nativeint i -> E.float (Nativeint.to_string i)
      | Const_float f -> E.float f (* TODO: preserve float *)
      | Const_string (i,_) (*TODO: here inline js*) -> 
        E.str i
    end

  | Const_pointer (c,pointer_info) -> 
    E.int ?comment:(Lam_compile_util.comment_of_pointer_info pointer_info)
      (Int32.of_int c )

  | Const_block(tag, tag_info, xs ) -> 
    Js_of_lam_block.make_block NA tag_info 
      (E.small_int  tag) (List.map translate xs)

  | Const_float_array ars -> 
    (* according to the compiler 
        const_float_array is immutable 
       {[ Lprim(Pccall prim_obj_dup, [master]) ]},
        however, we can not translate 
       {[ prim_obj_dup(x) =>  x' ]}
        since x' is now mutable, prim_obj_dup does a copy,

        the compiler does this  is mainly to extract common data into data section, 
        we  deoptimized this in js backend? so it is actually mutable 
    *)
    (* TODO-- *)
    Js_of_lam_array.make_array Mutable Pfloatarray 
      (List.map (fun x ->  E.float  x ) ars)
    (* E.arr Mutable ~comment:"float array" *)
    (*   (List.map (fun x ->  E.float  x ) ars) *)

  | Const_immstring s ->  (*TODO *)
    E.str s  (* TODO: check *)
