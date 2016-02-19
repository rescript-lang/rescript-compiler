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

let caml_raise_sys_error msg = 
  raise (Sys_error msg)

let caml_failwith s = raise (Failure s)

let caml_invalid_argument s = 
  raise (Invalid_argument s)



let caml_array_bound_error () =
  raise (Invalid_argument "index out of bounds") 

let caml_raise_zero_divide () = 
  raise Division_by_zero

let caml_raise_not_found () = 
  raise Not_found

let caml_undef_module loc = raise (Undefined_recursive_module loc) 
