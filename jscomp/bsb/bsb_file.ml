



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
      | {st_mtime = output_stamp}, {st_mtime = input_stamp} when input_stamp <= output_stamp 
        -> false
      | _ -> copy_with_permission input_name output_name; true 
      | exception _ -> copy_with_permission input_name output_name; true
    else false
