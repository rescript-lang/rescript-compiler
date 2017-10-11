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

type t = Types.signature

let empty  = []
let length  = List.length 
let name_of_signature_item (x : Types.signature_item )=
  match x with 
  | Sig_value (i,_) 
  | Sig_module (i,_,_) -> i 
  | Sig_typext (i,_,_) -> i 
  | Sig_modtype(i,_) -> i 
  | Sig_class (i,_,_) -> i 
  | Sig_class_type(i,_,_) -> i 
  | Sig_type(i,_,_) -> i   


(** It should be safe to replace Pervasives[], 
    we should test cases  like module aliases if it is serializable or not 
    {[ module Pervasives = List ]} 
*)
let serializable_signature (x : Types.signature_item) =
  match x with 
  | Sig_value(_, {val_kind = Val_prim _}) -> false
  | Sig_typext _ 
  | Sig_module _
  | Sig_class _ 
  | Sig_value _ -> true
  | Sig_modtype _ | Sig_class_type _ | Sig_type _ -> false



let filter_serializable_signatures signature  : t  = 
  List.filter serializable_signature signature
  
(* Input path is a global module 
    TODO: it should be fine for local module
*)
let find_serializable_signatures_by_path v (env : Env.t) 
  : t option = 
  match Env.find_module (Pident v) env with 
  | exception Not_found -> None 
  | {md_type = Mty_signature signature; _} -> 
    Some (filter_serializable_signatures signature)
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
let get_name  (serializable_sigs : t) (pos : int) = 
  Ident.name (name_of_signature_item (List.nth  serializable_sigs  pos))






