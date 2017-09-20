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



type error = 
  | Package_not_found of string * string option (* json file *)
  | Json_config of Ext_position.t * string

exception Error of error 

let error err = raise (Error err)
let package_not_found ~pkg ~json = 
  error (Package_not_found(pkg,json))

let to_string (x : error) = 
  match x with     
  | Package_not_found (name,json_opt) -> 
    let in_json = match json_opt with None -> Ext_string.empty | Some x -> " in " ^ x in 
    if Ext_string.equal name Bs_version.package_name then 
      Printf.sprintf "Package bs-platform is not found %s , it is the basic package required, if you have it installed globally\n\
                      Please run 'npm link bs-platform' to make it available " in_json
    else 
      Printf.sprintf 
        "BuckleScript package %s not found or built %s, if it is not built\n\
         Please run 'bsb -make-world', otherwise please install it " name in_json

  | Json_config (pos,s) ->
    Format.asprintf "File bsconfig.json (%a) : %s \n\
                     For more details, please checkout the schema http://bucklescript.github.io/bucklescript/docson/#build-schema.json 
        " Ext_position.print pos s 


let failf ~loc fmt =
  let prefix =
    Format.asprintf "bsconfig.json %a: " Ext_position.print loc  in
  Format.ksprintf (fun s -> failwith (prefix ^ s)) fmt


let config_error config fmt =
  let loc = Ext_json.loc_of config in

  error (Json_config (loc,fmt))

let () = 
  Printexc.register_printer (fun x ->
      match x with 
      | Error x -> 
        Some (to_string x )
      | _ -> None
    )
