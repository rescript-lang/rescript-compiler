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

(** type definitions for external argument *)

type cst = 
  | Arg_int_lit of int 
  | Arg_string_lit of string 

  | Arg_js_null
  | Arg_js_true
  | Arg_js_false
  | Arg_js_json of string
type label = 
  | Label of string * cst option 
  | Empty of cst option
  | Optional of string 
  (* it will be ignored , side effect will be recorded *)

type attr = 
  | NullString of (int * string) list (* `a does not have any value*)
  | NonNullString of (int * string) list (* `a of int *)
  | Int of (int * int ) list (* ([`a | `b ] [@bs.int])*)
  | Arg_cst of cst
  | Fn_uncurry_arity of int (* annotated with [@bs.uncurry ] or [@bs.uncurry 2]*)
    (* maybe we can improve it as a combination of {!Asttypes.constant} and tuple *)
  | Array 
  | Extern_unit
  | Nothing
  | Ignore
  | Unwrap

type t = 
  {
    arg_type : attr;
    arg_label : label
  }


exception Error of Location.t * Ext_json_parse.error

let pp_invaild_json fmt err = 
  Format.fprintf fmt "@[Invalid json literal:  %a@]@." 
    Ext_json_parse.report_error err

let () = 
  Location.register_error_of_exn (function 
    | Error (loc,err) ->       
      Some (Location.error_of_printer loc pp_invaild_json err)
    | _ -> None
    )


let cst_json (loc : Location.t) s : cst  =
  match Ext_json_parse.parse_json_from_string s with 
  | True _ -> Arg_js_true
  | False _ -> Arg_js_false 
  | Null _ -> Arg_js_null 
  | _ -> Arg_js_json s 
  | exception Ext_json_parse.Error (start,finish,error_info)
    ->
    let loc1 = {
      loc with
       loc_start = 
        Ext_position.offset loc.loc_start start; 
       loc_end =   
       Ext_position.offset loc.loc_start finish;
    } in 
     raise (Error (loc1 , error_info))

let cst_int i = Arg_int_lit i 
let cst_string s = Arg_string_lit s 
let empty_label = Empty None 
let empty_lit s = Empty (Some s) 
let label s cst = Label(s,cst)
let optional s = Optional s 

let empty_kind arg_type = { arg_label = empty_label ; arg_type }
