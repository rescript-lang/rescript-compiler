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

[@@@warning "+9"]

type t =
  { 
    dir_or_files : string array ;
    st_mtimes : float array;
    source_directory :  string ;    
  }
(* float_of_string_opt *)
external hexstring_of_float : float -> int -> char -> string
  = "caml_hexstring_of_float"

let hex_of_float f = hexstring_of_float f (-1) '-'

(* This should not lose any preicision *)
(* let id (f : float) = 
    float_of_string (hex_of_float f) = f
 *)

let encode ( {source_directory ; st_mtimes; dir_or_files} : t ) 
    (buf: Ext_buffer.t)= 
  Ext_buffer.add_string_char buf Bs_version.version '\n';  
  Ext_buffer.add_string_char buf source_directory '\n';
  let dir_or_files_len = Array.length dir_or_files in 
  (if dir_or_files_len <> 0 then begin 
    Ext_buffer.add_string buf dir_or_files.(0);
    for i = 1 to dir_or_files_len - 1 do 
      Ext_buffer.add_char_string buf '\t' dir_or_files.(i) 
    done  
  end);
  Ext_buffer.add_char buf '\n';
  let st_mtimes_len = Array.length st_mtimes in 
  (if st_mtimes_len <> 0 then begin 
    Ext_buffer.add_string buf (hex_of_float st_mtimes.(0));
    for i = 1 to st_mtimes_len - 1 do 
      Ext_buffer.add_char_string buf '\t' (hex_of_float st_mtimes.(i))  
    done     
  end);
  Ext_buffer.add_char buf '\n'
  
let decode_exn ic  =
  let source_directory = input_line ic in 
  let dir_or_files = input_line ic in 
  let dir_or_files = 
    Array.of_list 
      (Ext_string.split dir_or_files '\t') in 
  let st_mtimes_line = 
    input_line ic in 
  let st_mtimes = 
    Ext_array.of_list_map 
      (Ext_string.split st_mtimes_line '\t')   
      (fun x -> float_of_string x) in 
  close_in ic ;
  {dir_or_files; st_mtimes; source_directory}


(* TODO: for such small data structure, maybe text format is better *)

let write (fname : string)  (x : t) =
  let buf = Ext_buffer.create 1_000 in   
  encode x buf;  
  let oc = open_out_bin fname in
  Ext_buffer.output_buffer oc buf ;
  close_out oc





type check_result =
  | Good
  | Bsb_file_corrupted
  | Bsb_file_not_exist (** We assume that it is a clean repo *)
  | Bsb_source_directory_changed
  | Bsb_bsc_version_mismatch
  | Bsb_forced
  | Other of string

let pp_check_result fmt (check_resoult : check_result) =
  Format.pp_print_string fmt (match check_resoult with
      | Good -> "OK"
      | Bsb_file_corrupted -> "Stored data corrupted"
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

    




let record ~per_proj_dir ~file  (file_or_dirs : string list) : unit =
  let dir_or_files = Array.of_list file_or_dirs in 
  let st_mtimes = 
    Ext_array.map dir_or_files
      (fun  x ->      
           (Unix.stat (Filename.concat per_proj_dir  x )).st_mtime
         )
  in 
  write file
    { 
      st_mtimes ;
      dir_or_files;
      source_directory = per_proj_dir ;
    }

(** check time stamp for all files
    TODO: those checks system call can be saved later
    Return a reason
    Even forced, we still need walk through a little
    bit in case we found a different version of compiler
*)
let check ~(per_proj_dir:string) ~forced ~file : check_result =
  match  open_in_bin file with   (* Windows binary mode*)    
  | exception _ -> Bsb_file_not_exist
  | ic ->
    if input_line ic <> Bs_version.version then Bsb_bsc_version_mismatch
    else 
      match decode_exn ic with 
      | exception _ -> Bsb_file_corrupted (* corrupted file *)
      | {
        dir_or_files ; source_directory; st_mtimes
      } ->
        if per_proj_dir <> source_directory then Bsb_source_directory_changed else
        if forced then Bsb_forced (* No need walk through *)
        else
          try
            check_aux per_proj_dir dir_or_files st_mtimes  0 (Array.length dir_or_files)
          with e ->
            begin
              Bsb_log.info
                "@{<info>Stat miss %s@}@."
                (Printexc.to_string e);
              Bsb_file_not_exist        
            end

