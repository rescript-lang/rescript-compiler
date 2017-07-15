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



type error 
  = Unsupported_predicates
  | Conflict_bs_bs_this_bs_meth  
  | Duplicated_bs_deriving
  | Conflict_attributes

  | Duplicated_bs_as 
  | Expect_int_literal
  | Expect_string_literal
  | Expect_int_or_string_or_json_literal
  | Unhandled_poly_type
  | Unregistered of string 
  | Invalid_underscore_type_in_external
  | Invalid_bs_string_type 
  | Invalid_bs_int_type 
  | Invalid_bs_unwrap_type
  | Conflict_ffi_attribute of string
  | Not_supported_in_bs_deriving
  | Canot_infer_arity_by_syntax
  | Illegal_attribute
  | Inconsistent_arity of int * int 
  (* we still rqeuire users to have explicit annotation to avoid
     {[ (((int -> int) -> int) -> int )]}
  *)
  | Not_supported_directive_in_bs_return
  | Expect_opt_in_bs_return_to_opt
  | Label_in_uncurried_bs_attribute

  | Bs_this_simple_pattern

let pp_error fmt err =
  Format.pp_print_string fmt @@ match err with
  | Label_in_uncurried_bs_attribute 
    -> "label is not allowed here, it is due to `bs.` attribute indicate uncurried calling convention which does not support label argument yet"
  | Expect_opt_in_bs_return_to_opt
      ->
        "bs.return directive *_to_opt expect return type to be \n\
         syntax wise `_ option` for safety"

  | Not_supported_directive_in_bs_return
    ->
    "Not supported return directive"                
  | Illegal_attribute ->
    "Illegal attributes"
  | Canot_infer_arity_by_syntax
    ->   "Can not infer the arity by syntax, either [@bs.uncurry n] or \n\
              write it in arrow syntax "
  | Inconsistent_arity (arity,n)
      -> Printf.sprintf "Inconsistent arity %d vs %d" arity n 
  | Not_supported_in_bs_deriving
    ->
    "not supported in deriving"
  | Unsupported_predicates 
    ->
     "unsupported predicates"
  | Conflict_bs_bs_this_bs_meth -> 
     "[@bs.this], [@bs], [@bs.meth] can not be applied at the same time"
  | Duplicated_bs_deriving
    -> "duplicated bs.deriving attribute"
  | Conflict_attributes
    -> "conflicting attributes " 
  | Expect_string_literal
    -> "expect string literal "
  | Duplicated_bs_as 
    -> 
    "duplicated bs.as "
  | Expect_int_literal 
    -> 
    "expect int literal "
  | Expect_int_or_string_or_json_literal
    ->
    "expect int or string literal or json literal ({json||json}) "
  | Unhandled_poly_type 
    -> 
    "Unhandled poly type"
  | Unregistered str 
    -> "Unregistered " ^ str 
  | Invalid_underscore_type_in_external
    ->
    "_ is not allowed in combination with external optional type"
  | Invalid_bs_string_type
    -> 
    "Not a valid  type for [@bs.string]"
  | Invalid_bs_int_type 
    -> 
    "Not a valid  type for [@bs.int]"
  | Invalid_bs_unwrap_type
    ->
    "Not a valid type for [@bs.unwrap]. Type must be an inline variant (closed), and\n\
     each constructor must have an argument."
  | Conflict_ffi_attribute str
    ->
    "Conflicting FFI attributes found: " ^ str
  | Bs_this_simple_pattern
    -> 
    "[@bs.this] expect its pattern variable to be simple form"

type exn +=  Error of Location.t * error


let () = 
  Location.register_error_of_exn (function
    | Error(loc,err) -> 
      Some (Location.error_of_printer loc pp_error err)
    | _ -> None
    )

let err loc error = raise (Error(loc, error))
