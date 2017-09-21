(* Copyright (C) 2017 Authors of BuckleScript
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


type warning_error = 
  | Warn_error_false 
  (* default [false] to make our changes non-intrusive *)
  | Warn_error_true
  | Warn_error_number of string 

type t = {
  number : string option;
  error : warning_error
}



let warning_number =  "-40+6+7+27+32..39+44+45"

let default_warning_flag = 
  "-w -40+6+7+27+32..39+44+45"

let warn_error = " -warn-error A"
let warning_to_string no_dev 
    (warning ) : string = 
  if no_dev then 
    match warning.number with 
    | None ->
      default_warning_flag      
    | Some x -> 
      "-w " ^ x 
  else 
    match warning.number with 
    | None -> 
      (match warning.error with 
       | Warn_error_true -> default_warning_flag ^ warn_error
       | Warn_error_number x -> 
         default_warning_flag ^ " -warn-error " ^ x
       | Warn_error_false -> default_warning_flag
      )
    | Some x -> 
      "-w " ^ x ^
      (match warning.error with 
       | Warn_error_true -> 
         warn_error
       | Warn_error_false -> 
         Ext_string.empty
       | Warn_error_number y -> 
         " -warn-error " ^ y
      )

let opt_warning_to_string no_dev warning =       
  match warning with 
  | None -> default_warning_flag
  | Some w -> warning_to_string no_dev w 

let from_map (m : Ext_json_types.t String_map.t) = 
  let number_opt = String_map.find_opt Bsb_build_schemas.number m  in 
  let error_opt = String_map.find_opt Bsb_build_schemas.error m  in 
  match number_opt, error_opt  with 
  | None, None -> None
  | _, _ -> 
    let error  = 
      match error_opt with 
      | Some (True _) -> Warn_error_true
      | Some (False _) -> Warn_error_false
      | Some (Str {str ; }) 
        -> Warn_error_number str 
      | Some x -> Bsb_exception.config_error x "expect true/false or string"
      | None -> Warn_error_false
      (** To make it less intrusive : warning error has to be enabled*)  
    in
    let number = 
      match number_opt with   
      | Some (Str { str = number}) -> Some number
      | None -> None 
      | Some x -> Bsb_exception.config_error x "expect a string" 
    in 
    Some {number; error }