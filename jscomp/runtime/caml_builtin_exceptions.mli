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




(** *)

type exception_block =  string * nativeint
val out_of_memory : exception_block               (* [@@dead "out_of_memory"] *)
val sys_error  : exception_block                                 (* [@@dead "sys_error"] *)
val failure : exception_block                                   (* [@@dead "failure"] *)
val invalid_argument : exception_block                          (* [@@dead "invalid_argument"] *)
val end_of_file : exception_block                               (* [@@dead "end_of_file"] *)
val division_by_zero : exception_block                          (* [@@dead "division_by_zero"] *)
val not_found : exception_block                                 (* [@@dead "not_found"] *)
val match_failure : exception_block                             (* [@@dead "match_failure"] *)
val stack_overflow : exception_block                            (* [@@dead "stack_overflow"] *)
val sys_blocked_io : exception_block                            (* [@@dead "sys_blocked_io"] *)
val assert_failure : exception_block                           (* [@@dead "assert_failure"] *)
val undefined_recursive_module : exception_block               (* [@@dead "undefined_recursive_module"] *)


 