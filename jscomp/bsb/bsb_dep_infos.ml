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

type dep_info = {
  dir_or_file : string ;
  stamp : float 
}

type t = 
  { file_stamps : dep_info array ; 
    source_directory :  string ;
    bsb_version : string
  }


let magic_number = "BS_DEP_INFOS_20161116"
let bsb_version = "20161116+dev"

let write (fname : string)  (x : t) = 
  let oc = open_out_bin fname in 
  output_string oc magic_number ;
  output_value oc x ; 
  close_out oc 

let read (fname : string) : t = 
  let ic = open_in_bin fname in  (* Windows binary mode*)
  let buffer = really_input_string ic (String.length magic_number) in
  assert (buffer = magic_number);
  let res : t = input_value ic  in 
  close_in ic ; 
  res



let no_need_regenerate = ""


let rec check_aux xs i finish = 
  if i = finish then no_need_regenerate
  else 
    let k = Array.unsafe_get  xs i  in
    let current_file = k.dir_or_file in
    let stat = Unix.stat  current_file in 
    if stat.st_mtime <= k.stamp then 
      check_aux xs (i + 1 ) finish 
    else current_file

(** check time stamp for all files 
    TODO: those checks system call can be saved later
    Return a reason 
*)
let check ~cwd file =
  try 
    let {file_stamps = xs; source_directory; bsb_version = old_version} = read file  in 
    if old_version <> bsb_version then old_version ^ " -> " ^ bsb_version else
    if cwd <> source_directory then source_directory ^ " -> " ^ cwd else
      check_aux xs  0 (Array.length xs)  
  with _ -> file ^ " does not exist"

let store ~cwd name file_stamps = 
  write name { file_stamps ; source_directory = cwd ; bsb_version }
