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

(**
   This module is used to check whether [build.ninja] needs
   be regenerated. Everytime [bsb] run [regenerate_ninja], 
   bsb will try to [check] if it is needed, 
   if needed, bsb will regenerate ninja file and store the 
   metadata again
*)





type check_result = 
  | Good
  | Bsb_file_not_exist (** We assume that it is a clean repo *)  
  | Bsb_source_directory_changed
  | Bsb_bsc_version_mismatch  
  | Bsb_forced
  | Other of string

val pp_check_result : Format.formatter -> check_result -> unit


(** [record cwd file relevant_file_or_dirs]
    The data structure we decided to whether regenerate [build.ninja] 
    or not. 
    Note that if we don't record absolute path,  ninja will not notice  its build spec changed, 
    it will not trigger  rebuild behavior, 
    It may not be desired behavior, since there is some subtlies here (__FILE__ or __dirname)

    We serialize such data structure and call {!check} to decide
    [build.ninja] should be regenerated
*)
val record : cwd:string -> file:string -> string list -> unit


(** check if [build.ninja] should be regenerated *)
val check :
  cwd:string ->  
  forced:bool -> file:string -> check_result
