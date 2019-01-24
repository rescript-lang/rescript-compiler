(* This is a terrible hack that plays on the internal representation
   of file descriptors.  The result is a number (as a string)
   that the fdstatus.exe auxiliary program can use to check whether
   the fd is open. *)

let string_of_fd (fd: Unix.file_descr) : string =
  match Sys.os_type with
  | "Unix" | "Cygwin" ->  string_of_int (Obj.magic fd : int)
  | "Win32" ->
      if Sys.word_size = 32 then
        Int32.to_string (Obj.magic fd : int32)
      else
        Int64.to_string (Obj.magic fd : int64)
  | _ -> assert false

let _ =
  let f0 = Unix.(openfile "tmp.txt" [O_WRONLY; O_CREAT; O_TRUNC] 0o600) in
  let f1 = Unix.(openfile "tmp.txt" [O_RDONLY; O_KEEPEXEC] 0) in
  let f2 = Unix.(openfile "tmp.txt" [O_RDONLY; O_CLOEXEC] 0) in
  let d0 = Unix.dup f0 in
  let d1 = Unix.dup ~cloexec:false f1 in
  let d2 = Unix.dup ~cloexec:true f2 in
  let (p0, p0') = Unix.pipe () in
  let (p1, p1') = Unix.pipe ~cloexec:false () in
  let (p2, p2') = Unix.pipe ~cloexec:true () in
  let s0 = Unix.(socket PF_INET SOCK_STREAM 0) in
  let s1 = Unix.(socket ~cloexec:false PF_INET SOCK_STREAM 0) in
  let s2 = Unix.(socket ~cloexec:true PF_INET SOCK_STREAM 0) in
  let (x0, x0') =
    try Unix.(socketpair PF_UNIX SOCK_STREAM 0)
    with Invalid_argument _ -> (p0, p0') in
    (* socketpair not available under Win32; keep the same output *)
  let (x1, x1') =
    try Unix.(socketpair ~cloexec:false PF_UNIX SOCK_STREAM 0)
    with Invalid_argument _ -> (p1, p1') in
  let (x2, x2') =
    try Unix.(socketpair ~cloexec:true PF_UNIX SOCK_STREAM 0)
    with Invalid_argument _ -> (p2, p2') in

  let fds = [| f0;f1;f2; d0;d1;d2;
               p0;p0';p1;p1';p2;p2';
               s0;s1;s2;
               x0;x0';x1;x1';x2;x2' |] in
  let pid =
    Unix.create_process
      (Filename.concat Filename.current_dir_name "fdstatus.exe")
      (Array.append [| "fdstatus" |] (Array.map string_of_fd fds))
      Unix.stdin Unix.stdout Unix.stderr in
  ignore (Unix.waitpid [] pid);
  Array.iter (fun fd -> try Unix.close fd with Unix.Unix_error _ -> ()) fds;
  Sys.remove "tmp.txt"
