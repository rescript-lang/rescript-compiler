(* Copyright (C) 2018 - Hongbo Zhang, Authors of ReScript
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




let set_infos filename (infos : Unix.stats) =
  Unix.utimes filename infos.st_atime infos.st_mtime;
  Unix.chmod filename infos.st_perm
(** it is not necessary to call [chown] since it is within the same user 
    and {!Unix.chown} is not implemented under Windows
*)
  (*
  try
    Unix.chown filename infos.st_uid infos.st_gid
  with Unix_error(EPERM,_,_) -> ()
*)

let buffer_size = 8192;;
let buffer = Bytes.create buffer_size;;

let file_copy input_name output_name =
  let fd_in = Unix.openfile input_name [O_RDONLY] 0 in
  let fd_out = Unix.openfile output_name [O_WRONLY; O_CREAT; O_TRUNC] 0o666 in
  let rec copy_loop () =
    match Unix.read fd_in buffer 0 buffer_size with
    |  0 -> ()
    | r -> ignore (Unix.write fd_out buffer 0 r); copy_loop ()
  in
  copy_loop ();
  Unix.close fd_in;
  Unix.close fd_out;;


let copy_with_permission input_name output_name =
  file_copy input_name output_name ;
  set_infos output_name (Unix.lstat input_name)  

let install_if_exists ~destdir input_name = 
  if Sys.file_exists input_name then 
    let output_name = (Filename.concat destdir (Filename.basename input_name)) in
    match Unix.stat output_name , Unix.stat input_name with
    | {st_mtime = output_stamp;_}, {st_mtime = input_stamp;_} when input_stamp <= output_stamp 
      -> false
    | _ -> copy_with_permission input_name output_name; true 
    | exception _ -> copy_with_permission input_name output_name; true
  else false
