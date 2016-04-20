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

type exception_block = string * nativeint 


let out_of_memory =  "Out_of_memory", 0n
let sys_error = "Sys_error", -1n
let failure =  "Failure", -2n
let invalid_argument =  "Invalid_argument", -3n
let end_of_file = "End_of_file",-4n
let division_by_zero =  "Division_by_zero", -5n
let not_found = "Not_found", -6n
let match_failure =  "Match_failure", -7n
let stack_overflow =  "Stack_overflow",-8n
let sys_blocked_io =  "Sys_blocked_io", -9n
let assert_failure =  "Assert_failure", -10n
let undefined_recursive_module =  "Undefined_recursive_module", -11n

(* TODO: 
   1. is it necessary to tag [248] here
   2. is it okay to remove the negative value
*)
