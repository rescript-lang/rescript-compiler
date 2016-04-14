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

type exception_block = int * string * int 
let out_of_memory = 248, "Out_of_memory", 0
let sys_error = 248, "Sys_error", -1
let failure = 248, "Failure", -2
let invalid_argument = 248, "Invalid_argument", -3
let end_of_file = 248, "End_of_file",-4
let division_by_zero = 248, "Division_by_zero", -5
let not_found = 248, "Not_found", -6
let match_failure = 248, "Match_failure", -7
let stack_overflow = 248, "Stack_overflow",-8
let sys_blocked_io = 248, "Sys_blocked_io", -9
let assert_failure = 248, "Assert_failure", -10
let undefined_recursive_module = 248, "Undefined_recursive_module", -11



(* Exported for better inlining *)
(* It's common that we have <code> a = caml_set_oo_id([248,"string",0])</code> *)
(* This can be inlined as <code> a = caml_set_oo_id([248,"tag", caml_oo_last_id++])</code> *)
 (* @type {number} *)

let id = ref 0n


(* see  #251
   {[
     CAMLprim value caml_set_oo_id (value obj) {
       Field(obj, 1) = oo_last_id;
       oo_last_id += 2;
       return obj;
     }

   ]}*)
let caml_set_oo_id (b : exception_block)  = 
    Obj.set_field (Obj.repr b) 1 (Obj.repr !id);
    id := Nativeint.add !id  1n; 
    b

let get_id () = 
  id := Nativeint.add !id 1n; !id
