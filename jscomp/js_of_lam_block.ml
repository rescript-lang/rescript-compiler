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

(* TODO: it would be even better, if the [tag_info] contains more information
   about immutablility
 *)
let make_block mutable_flag (tag_info : Lambda.tag_info) tag args  = 

  match mutable_flag, tag_info with
  | _, Array -> Js_of_lam_array.make_array mutable_flag  Pgenarray args
  | _ , _ -> E.make_block tag tag_info args mutable_flag
  (* | _, (  Tuple | Variant _ ) -> (\** TODO: check with inline record *\) *)
  (*     E.arr Immutable *)
  (*       (E.small_int  ?comment:(Lam_compile_util.comment_of_tag_info tag_info) tag   *)
  (*        :: args) *)
  (* | _, _  ->  *)
  (*     E.arr mutable_flag *)
  (*       (E.int  ?comment:(Lam_compile_util.comment_of_tag_info tag_info) tag   *)
  (*        :: args) *)

let field e i = E.index e i

let set_field e i e0 = (E.assign (E.index e i)  e0)




