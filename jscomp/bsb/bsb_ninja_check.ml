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


type t =
  { 
    dir_or_files : string array ;
    st_mtimes : float array;
    source_directory :  string ;    
  }


let magic_number = Bs_version.version

(* TODO: for such small data structure, maybe text format is better *)

let write (fname : string)  (x : t) =
  let oc = open_out_bin fname in
  output_string oc magic_number ;
  output_value oc x ;
  close_out oc





type check_result =
  | Good
  | Bsb_file_not_exist (** We assume that it is a clean repo *)
  | Bsb_source_directory_changed
  | Bsb_bsc_version_mismatch
  | Bsb_forced
  | Other of string

let pp_check_result fmt (check_resoult : check_result) =
  Format.pp_print_string fmt (match check_resoult with
      | Good -> "OK"
      | Bsb_file_not_exist -> "Dependencies information missing"
      | Bsb_source_directory_changed ->
        "Bsb source directory changed"
      | Bsb_bsc_version_mismatch ->
        "Bsc or bsb version mismatch"
      | Bsb_forced ->
        "Bsb forced rebuild"
      | Other s -> s)

let rec check_aux cwd (xs : string array) (ys: float array) i finish =
  if i = finish then Good
  else
    let current_file = Array.unsafe_get  xs i  in
    
    let stat = Unix.stat  (Filename.concat cwd  current_file) in
    if stat.st_mtime <= Array.unsafe_get ys i then
      check_aux cwd xs ys (i + 1 ) finish
    else Other current_file


let read (fname : string) (cont : t -> check_result) =
  match open_in_bin fname with   (* Windows binary mode*)
  | ic ->
    let buffer = really_input_string ic (String.length magic_number) in
    if (buffer <> magic_number) then Bsb_bsc_version_mismatch
    else
      let res : t = input_value ic  in
      close_in ic ;
      cont res
  | exception _ -> Bsb_file_not_exist

let record ~cwd ~file  (file_or_dirs : string list) : unit =
  let dir_or_files = Array.of_list file_or_dirs in 
  let st_mtimes = 
    Ext_array.map dir_or_files
      (fun  x ->      
           (Unix.stat (Filename.concat cwd  x )).st_mtime
         )
  in 
  write file
    { st_mtimes ;
      dir_or_files;
      source_directory = cwd ;
    }

(** check time stamp for all files
    TODO: those checks system call can be saved later
    Return a reason
    Even forced, we still need walk through a little
    bit in case we found a different version of compiler
*)
let check ~cwd ~forced ~file : check_result =
  read file  (fun  {
      dir_or_files ; source_directory; st_mtimes
    } ->
      if cwd <> source_directory then Bsb_source_directory_changed else
      if forced then Bsb_forced (* No need walk through *)
      else
        try
          check_aux cwd dir_or_files st_mtimes  0 (Array.length dir_or_files)
        with e ->
          begin
            Bsb_log.info
              "@{<info>Stat miss %s@}@."
              (Printexc.to_string e);
            Bsb_file_not_exist        
          end)

