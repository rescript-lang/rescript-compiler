
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

type case = bool
(** true means upper case*)

type ml_info =
  | Ml_source  (*  Ml_source(is_re, case) default to false  *)
  | Ml_empty
type mli_info = 
  | Mli_source 
  | Mli_empty

type module_info = 
  {
    mli_info : mli_info ; 
    ml_info : ml_info ; 
    dir : string ; 
    is_re : bool;
    case : bool;
    name_sans_extension : string  ;
  }


type t = module_info String_map.t 

type ts = t array 
(** indexed by the group *)





let filename_sans_suffix_of_module_info (x : module_info) =
  x.name_sans_extension




let has_reason_files (map  : t ) = 
  String_map.exists map (fun _ {is_re} -> is_re)  



