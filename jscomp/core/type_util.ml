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



(* Input path is a global module 
    TODO: it should be fine for local module
*)
let find_serializable_signatures_by_path v (env : Env.t) 
  : Types.signature option = 
  match Env.find_module (Pident v) env with 
  | exception Not_found -> None 
  | {md_type = Mty_signature signature; _} -> 
    Some (Type_int_to_string.filter_serializable_signatures signature)
  (** TODO: refine *)
  | _ -> Ext_log.err __LOC__  "@[impossible path %s@]@."
           (Ident.name v) ; assert false 

let rec dump_summary fmt (x : Env.summary) = 
  match x with 
  | Env_empty -> ()
  | Env_value(s,id,value_description) -> 
    dump_summary fmt s ;
    Printtyp.value_description id fmt value_description
  | _ -> ()

(** Used in [ Lglobal_module] *)
let get_name  (serializable_sigs : Types.signature) (pos : int) = 
  Ident.name @@ Type_int_to_string.name_of_signature_item @@ List.nth  serializable_sigs  pos

(* let find_name id pos env = *)
(*   match find_serializable_signatures_by_path id env with *)
(*   | Some signatures -> *)
(*     Some (get_name signatures pos) *)
(*   | None -> None       *)



    

