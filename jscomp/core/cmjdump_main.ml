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

let p = Format.fprintf 

let pp_cmj fmt 
    ({ values ; effect; npm_package_path } :Js_cmj_format.t) = 
  p fmt "@[package info: %a@]@."  
    Js_config.dump_packages_info
    npm_package_path
    ;
  p fmt "@[effect: %a@]@."
    (fun fmt o ->
       match o with None -> ()
                  | Some s -> p fmt "None pure due to %s" s 
    ) effect ;
  p fmt "@[arities: @[%a@]@]@."
    (fun fmt m -> 
       m |> String_map.iter (fun k (v : Js_cmj_format.cmj_value) -> 
           match v.arity with 
           | Single arity ->
             p fmt "@[%s:@ @[%a@]@]@." k Lam_arity.print arity
           | Submodule xs -> 
             p fmt "@[<h 1>@[%s:@ @[<hov 2>%a@]@]@]" k 
               (fun fmt xs ->
                  Array.iter (fun arity -> p fmt "@[%a@]@ ;" Lam_arity.print arity ) 
                    xs) xs 

         )) values



let () = 
  match Sys.argv  with
  | [|_; file |] 
    -> pp_cmj Format.std_formatter (Js_cmj_format.from_file file)
  | _ -> failwith "expect one argument"