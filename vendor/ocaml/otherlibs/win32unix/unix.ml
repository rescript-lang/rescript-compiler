(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*   Xavier Leroy and Pascal Cuoq, projet Cristal, INRIA Rocquencourt     *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Initialization *)

external startup: unit -> unit = "win_startup"
external cleanup: unit -> unit = "win_cleanup"

let _ = startup(); at_exit cleanup

(* Errors *)

type error =
  (* Errors defined in the POSIX standard *)
    E2BIG               (* Argument list too long *)
  | EACCES              (* Permission denied *)
  | EAGAIN              (* Resource temporarily unavailable; try again *)
  | EBADF               (* Bad file descriptor *)
  | EBUSY               (* Resource unavailable *)
  | ECHILD              (* No child process *)
  | EDEADLK             (* Resource deadlock would occur *)
  | EDOM                (* Domain error for math functions, etc. *)
  | EEXIST              (* File exists *)
  | EFAULT              (* Bad address *)
  | EFBIG               (* File too large *)
  | EINTR               (* Function interrupted by signal *)
  | EINVAL              (* Invalid argument *)
  | EIO                 (* Hardware I/O error *)
  | EISDIR              (* Is a directory *)
  | EMFILE              (* Too many open files by the process *)
  | EMLINK              (* Too many links *)
  | ENAMETOOLONG        (* Filename too long *)
  | ENFILE              (* Too many open files in the system *)
  | ENODEV              (* No such device *)
  | ENOENT              (* No such file or directory *)
  | ENOEXEC             (* Not an executable file *)
  | ENOLCK              (* No locks available *)
  | ENOMEM              (* Not enough memory *)
  | ENOSPC              (* No space left on device *)
  | ENOSYS              (* Function not supported *)
  | ENOTDIR             (* Not a directory *)
  | ENOTEMPTY           (* Directory not empty *)
  | ENOTTY              (* Inappropriate I/O control operation *)
  | ENXIO               (* No such device or address *)
  | EPERM               (* Operation not permitted *)
  | EPIPE               (* Broken pipe *)
  | ERANGE              (* Result too large *)
  | EROFS               (* Read-only file system *)
  | ESPIPE              (* Invalid seek e.g. on a pipe *)
  | ESRCH               (* No such process *)
  | EXDEV               (* Invalid link *)
  (* Additional errors, mostly BSD *)
  | EWOULDBLOCK         (* Operation would block *)
  | EINPROGRESS         (* Operation now in progress *)
  | EALREADY            (* Operation already in progress *)
  | ENOTSOCK            (* Socket operation on non-socket *)
  | EDESTADDRREQ        (* Destination address required *)
  | EMSGSIZE            (* Message too long *)
  | EPROTOTYPE          (* Protocol wrong type for socket *)
  | ENOPROTOOPT         (* Protocol not available *)
  | EPROTONOSUPPORT     (* Protocol not supported *)
  | ESOCKTNOSUPPORT     (* Socket type not supported *)
  | EOPNOTSUPP          (* Operation not supported on socket *)
  | EPFNOSUPPORT        (* Protocol family not supported *)
  | EAFNOSUPPORT        (* Address family not supported by protocol family *)
  | EADDRINUSE          (* Address already in use *)
  | EADDRNOTAVAIL       (* Can't assign requested address *)
  | ENETDOWN            (* Network is down *)
  | ENETUNREACH         (* Network is unreachable *)
  | ENETRESET           (* Network dropped connection on reset *)
  | ECONNABORTED        (* Software caused connection abort *)
  | ECONNRESET          (* Connection reset by peer *)
  | ENOBUFS             (* No buffer space available *)
  | EISCONN             (* Socket is already connected *)
  | ENOTCONN            (* Socket is not connected *)
  | ESHUTDOWN           (* Can't send after socket shutdown *)
  | ETOOMANYREFS        (* Too many references: can't splice *)
  | ETIMEDOUT           (* Connection timed out *)
  | ECONNREFUSED        (* Connection refused *)
  | EHOSTDOWN           (* Host is down *)
  | EHOSTUNREACH        (* No route to host *)
  | ELOOP               (* Too many levels of symbolic links *)
  | EOVERFLOW
  (* All other errors are mapped to EUNKNOWNERR *)
  | EUNKNOWNERR of int  (* Unknown error *)

exception Unix_error of error * string * string

let _ = Callback.register_exception "Unix.Unix_error"
                                    (Unix_error(E2BIG, "", ""))

external error_message : error -> string = "unix_error_message"

let handle_unix_error f arg =
  try
    f arg
  with Unix_error(err, fun_name, arg) ->
    prerr_string Sys.argv.(0);
    prerr_string ": \"";
    prerr_string fun_name;
    prerr_string "\" failed";
    if String.length arg > 0 then begin
      prerr_string " on \"";
      prerr_string arg;
      prerr_string "\""
    end;
    prerr_string ": ";
    prerr_endline (error_message err);
    exit 2

external environment : unit -> string array = "unix_environment"
(* On Win32 environment access is always considered safe. *)
let unsafe_environment = environment
external getenv: string -> string = "caml_sys_getenv"
external unsafe_getenv: string -> string = "caml_sys_unsafe_getenv"
external putenv: string -> string -> unit = "unix_putenv"

type process_status =
    WEXITED of int
  | WSIGNALED of int
  | WSTOPPED of int

type wait_flag =
    WNOHANG
  | WUNTRACED

type file_descr

external execv : string -> string array -> 'a = "unix_execv"
external execve : string -> string array -> string array -> 'a = "unix_execve"
external execvp : string -> string array -> 'a = "unix_execvp"
external execvpe : string -> string array -> string array -> 'a = "unix_execvpe"

external waitpid : wait_flag list -> int -> int * process_status
                 = "win_waitpid"
external getpid : unit -> int = "unix_getpid"

let fork () = invalid_arg "Unix.fork not implemented"
let wait () = invalid_arg "Unix.wait not implemented"
let getppid () = invalid_arg "Unix.getppid not implemented"
let nice _ = invalid_arg "Unix.nice not implemented"

(* Basic file input/output *)

external filedescr_of_fd : int -> file_descr = "win_handle_fd"

let stdin = filedescr_of_fd 0
let stdout = filedescr_of_fd 1
let stderr = filedescr_of_fd 2

type open_flag =
    O_RDONLY
  | O_WRONLY
  | O_RDWR
  | O_NONBLOCK
  | O_APPEND
  | O_CREAT
  | O_TRUNC
  | O_EXCL
  | O_NOCTTY
  | O_DSYNC
  | O_SYNC
  | O_RSYNC
  | O_SHARE_DELETE
  | O_CLOEXEC
  | O_KEEPEXEC

type file_perm = int

external openfile : string -> open_flag list -> file_perm -> file_descr
           = "unix_open"
external close : file_descr -> unit = "unix_close"
external unsafe_read : file_descr -> bytes -> int -> int -> int
                     = "unix_read"
external unsafe_write : file_descr -> bytes -> int -> int -> int
                      = "unix_write"
external unsafe_single_write : file_descr -> bytes -> int -> int -> int
                      = "unix_single_write"

let read fd buf ofs len =
  if ofs < 0 || len < 0 || ofs > Bytes.length buf - len
  then invalid_arg "Unix.read"
  else unsafe_read fd buf ofs len
let write fd buf ofs len =
  if ofs < 0 || len < 0 || ofs > Bytes.length buf - len
  then invalid_arg "Unix.write"
  else unsafe_write fd buf ofs len
let single_write fd buf ofs len =
  if ofs < 0 || len < 0 || ofs > Bytes.length buf - len
  then invalid_arg "Unix.single_write"
  else unsafe_single_write fd buf ofs len

let write_substring fd buf ofs len =
  write fd (Bytes.unsafe_of_string buf) ofs len

let single_write_substring fd buf ofs len =
  single_write fd (Bytes.unsafe_of_string buf) ofs len

(* Interfacing with the standard input/output library *)

external in_channel_of_descr: file_descr -> in_channel
   = "win_inchannel_of_filedescr"
external out_channel_of_descr: file_descr -> out_channel
   = "win_outchannel_of_filedescr"
external descr_of_in_channel : in_channel -> file_descr
   = "win_filedescr_of_channel"
external descr_of_out_channel : out_channel -> file_descr
   = "win_filedescr_of_channel"

(* Seeking and truncating *)

type seek_command =
    SEEK_SET
  | SEEK_CUR
  | SEEK_END

external lseek : file_descr -> int -> seek_command -> int = "unix_lseek"

let truncate _name _len = invalid_arg "Unix.truncate not implemented"
let ftruncate _fd _len = invalid_arg "Unix.ftruncate not implemented"

(* File statistics *)

type file_kind =
    S_REG
  | S_DIR
  | S_CHR
  | S_BLK
  | S_LNK
  | S_FIFO
  | S_SOCK

type stats =
  { st_dev : int;
    st_ino : int;
    st_kind : file_kind;
    st_perm : file_perm;
    st_nlink : int;
    st_uid : int;
    st_gid : int;
    st_rdev : int;
    st_size : int;
    st_atime : float;
    st_mtime : float;
    st_ctime : float }

external stat : string -> stats = "unix_stat"
external lstat : string -> stats = "unix_lstat"
external fstat : file_descr -> stats = "unix_fstat"
external isatty : file_descr -> bool = "unix_isatty"

(* Operations on file names *)

external unlink : string -> unit = "unix_unlink"
external rename : string -> string -> unit = "unix_rename"
external link : string -> string -> unit = "unix_link"

(* Operations on large files *)

module LargeFile =
  struct
    external lseek : file_descr -> int64 -> seek_command -> int64
       = "unix_lseek_64"
    let truncate _name _len =
      invalid_arg "Unix.LargeFile.truncate not implemented"
    let ftruncate _name _len =
      invalid_arg "Unix.LargeFile.ftruncate not implemented"
    type stats =
      { st_dev : int;
        st_ino : int;
        st_kind : file_kind;
        st_perm : file_perm;
        st_nlink : int;
        st_uid : int;
        st_gid : int;
        st_rdev : int;
        st_size : int64;
        st_atime : float;
        st_mtime : float;
        st_ctime : float;
      }
    external stat : string -> stats = "unix_stat_64"
    external lstat : string -> stats = "unix_lstat_64"
    external fstat : file_descr -> stats = "unix_fstat_64"
  end

(* Mapping files into memory *)

external map_internal:
   file_descr -> ('a, 'b) CamlinternalBigarray.kind
              -> 'c CamlinternalBigarray.layout
              -> bool -> int array -> int64
              -> ('a, 'b, 'c) CamlinternalBigarray.genarray
     = "caml_unix_map_file_bytecode" "caml_unix_map_file"

let map_file fd ?(pos=0L) kind layout shared dims =
  map_internal fd kind layout shared dims pos

(* File permissions and ownership *)

type access_permission =
    R_OK
  | W_OK
  | X_OK
  | F_OK

external chmod : string -> file_perm -> unit = "unix_chmod"
let fchmod _fd _perm = invalid_arg "Unix.fchmod not implemented"
let chown _file _perm = invalid_arg "Unix.chown not implemented"
let fchown _fd _perm = invalid_arg "Unix.fchown not implemented"
let umask _msk = invalid_arg "Unix.umask not implemented"

external access : string -> access_permission list -> unit = "unix_access"

(* Operations on file descriptors *)

external dup : ?cloexec: bool -> file_descr -> file_descr = "unix_dup"
external dup2 :
   ?cloexec: bool -> file_descr -> file_descr -> unit = "unix_dup2"

external set_nonblock : file_descr -> unit = "unix_set_nonblock"
external clear_nonblock : file_descr -> unit = "unix_clear_nonblock"

external set_close_on_exec : file_descr -> unit = "win_set_close_on_exec"
external clear_close_on_exec : file_descr -> unit = "win_clear_close_on_exec"

(* Directories *)

external mkdir : string -> file_perm -> unit = "unix_mkdir"
external rmdir : string -> unit = "unix_rmdir"
external chdir : string -> unit = "unix_chdir"
external getcwd : unit -> string = "unix_getcwd"
let chroot _ = invalid_arg "Unix.chroot not implemented"

type dir_entry =
    Dir_empty
  | Dir_read of string
  | Dir_toread

type dir_handle =
  { dirname: string; mutable handle: int; mutable entry_read: dir_entry }

external findfirst : string -> string * int = "win_findfirst"
external findnext : int -> string= "win_findnext"

let opendir dirname =
  try
    let (first_entry, handle) = findfirst (Filename.concat dirname "*.*") in
    { dirname = dirname; handle = handle; entry_read = Dir_read first_entry }
  with End_of_file ->
    { dirname = dirname; handle = 0; entry_read = Dir_empty }

let readdir d =
  match d.entry_read with
    Dir_empty -> raise End_of_file
  | Dir_read name -> d.entry_read <- Dir_toread; name
  | Dir_toread -> findnext d.handle

external win_findclose : int -> unit = "win_findclose"

let closedir d =
  match d.entry_read with
    Dir_empty -> ()
  | _ -> win_findclose d.handle

let rewinddir d =
  closedir d;
  try
    let (first_entry, handle) = findfirst (d.dirname ^ "\\*.*") in
    d.handle <- handle; d.entry_read <- Dir_read first_entry
  with End_of_file ->
    d.handle <- 0; d.entry_read <- Dir_empty

(* Pipes *)

external pipe :
  ?cloexec: bool -> unit -> file_descr * file_descr = "unix_pipe"

let mkfifo _name _perm = invalid_arg "Unix.mkfifo not implemented"

(* Symbolic links *)

external readlink : string -> string = "unix_readlink"
external symlink_stub : bool -> string -> string -> unit = "unix_symlink"

(* See https://caml.inria.fr/mantis/view.php?id=7564.
   The Windows API used to create symbolic links does not normalize the target
   of a symbolic link, so we do it here.  Note that we cannot use the native
   Windows call GetFullPathName to do this because we need relative paths to
   stay relative. *)
let normalize_slashes path =
  if String.length path >= 4 && path.[0] = '\\' && path.[1] = '\\' && path.[2] = '?' && path.[3] = '\\' then
    path
  else
    String.init (String.length path) (fun i -> match path.[i] with '/' -> '\\' | c -> c)

let symlink ?to_dir source dest =
  let to_dir =
    match to_dir with
      Some to_dir ->
        to_dir
    | None ->
        try
          LargeFile.((stat source).st_kind = S_DIR)
        with _ ->
          false
  in
  let source = normalize_slashes source in
  symlink_stub to_dir source dest

external has_symlink : unit -> bool = "unix_has_symlink"

(* Locking *)

type lock_command =
    F_ULOCK
  | F_LOCK
  | F_TLOCK
  | F_TEST
  | F_RLOCK
  | F_TRLOCK

external lockf : file_descr -> lock_command -> int -> unit = "unix_lockf"

external terminate_process: int -> bool = "win_terminate_process"

let kill pid signo =
  if signo <> Sys.sigkill then
    invalid_arg "Unix.kill"
  else
    if not (terminate_process pid) then
      raise(Unix_error(ESRCH, "kill", ""))
        (* could be more precise *)

type sigprocmask_command = SIG_SETMASK | SIG_BLOCK | SIG_UNBLOCK
let sigprocmask _cmd _sigs = invalid_arg "Unix.sigprocmask not implemented"
let sigpending () = invalid_arg "Unix.sigpending not implemented"
let sigsuspend _sigs = invalid_arg "Unix.sigsuspend not implemented"
let pause () = invalid_arg "Unix.pause not implemented"

(* Time functions *)

type process_times =
  { tms_utime : float;
    tms_stime : float;
    tms_cutime : float;
    tms_cstime : float }

type tm =
  { tm_sec : int;
    tm_min : int;
    tm_hour : int;
    tm_mday : int;
    tm_mon : int;
    tm_year : int;
    tm_wday : int;
    tm_yday : int;
    tm_isdst : bool }

external time : unit -> float = "unix_time"
external gettimeofday : unit -> float = "unix_gettimeofday"
external gmtime : float -> tm = "unix_gmtime"
external localtime : float -> tm = "unix_localtime"
external mktime : tm -> float * tm = "unix_mktime"
let alarm _n = invalid_arg "Unix.alarm not implemented"
external sleepf : float -> unit = "unix_sleep"
let sleep n = sleepf (float n)
external times: unit -> process_times = "unix_times"
external utimes : string -> float -> float -> unit = "unix_utimes"

type interval_timer =
    ITIMER_REAL
  | ITIMER_VIRTUAL
  | ITIMER_PROF

type interval_timer_status =
  { it_interval: float;
    it_value: float }

let getitimer _it = invalid_arg "Unix.getitimer not implemented"
let setitimer _it _tm = invalid_arg "Unix.setitimer not implemented"

(* User id, group id *)

let getuid () = 1
let geteuid = getuid
let setuid _id = invalid_arg "Unix.setuid not implemented"

let getgid () = 1
let getegid = getgid
let setgid _id = invalid_arg "Unix.setgid not implemented"

let getgroups () = [|1|]
let setgroups _ = invalid_arg "Unix.setgroups not implemented"
let initgroups _ _ = invalid_arg "Unix.initgroups not implemented"

type passwd_entry =
  { pw_name : string;
    pw_passwd : string;
    pw_uid : int;
    pw_gid : int;
    pw_gecos : string;
    pw_dir : string;
    pw_shell : string }

type group_entry =
  { gr_name : string;
    gr_passwd : string;
    gr_gid : int;
    gr_mem : string array }

let getlogin () = try Sys.getenv "USERNAME" with Not_found -> ""
let getpwnam _x = raise Not_found
let getgrnam = getpwnam
let getpwuid = getpwnam
let getgrgid = getpwnam

(* Internet addresses *)

type inet_addr = string

let is_inet6_addr s = String.length s = 16

external inet_addr_of_string : string -> inet_addr
                                    = "unix_inet_addr_of_string"
external string_of_inet_addr : inet_addr -> string
                                    = "unix_string_of_inet_addr"

let inet_addr_any = inet_addr_of_string "0.0.0.0"
let inet_addr_loopback = inet_addr_of_string "127.0.0.1"
let inet6_addr_any =
  try inet_addr_of_string "::" with Failure _ -> inet_addr_any
let inet6_addr_loopback =
  try inet_addr_of_string "::1" with Failure _ -> inet_addr_loopback

(* Sockets *)

type socket_domain =
    PF_UNIX
  | PF_INET
  | PF_INET6

type socket_type =
    SOCK_STREAM
  | SOCK_DGRAM
  | SOCK_RAW
  | SOCK_SEQPACKET

type sockaddr =
    ADDR_UNIX of string
  | ADDR_INET of inet_addr * int

let domain_of_sockaddr = function
    ADDR_UNIX _ -> PF_UNIX
  | ADDR_INET(a, _) -> if is_inet6_addr a then PF_INET6 else PF_INET

type shutdown_command =
    SHUTDOWN_RECEIVE
  | SHUTDOWN_SEND
  | SHUTDOWN_ALL

type msg_flag =
    MSG_OOB
  | MSG_DONTROUTE
  | MSG_PEEK

external socket :
  ?cloexec: bool -> socket_domain -> socket_type -> int -> file_descr
  = "unix_socket"
let socketpair ?cloexec:_ _dom _ty _proto = invalid_arg "Unix.socketpair not implemented"
external accept :
  ?cloexec: bool -> file_descr -> file_descr * sockaddr = "unix_accept"
external bind : file_descr -> sockaddr -> unit = "unix_bind"
external connect : file_descr -> sockaddr -> unit = "unix_connect"
external listen : file_descr -> int -> unit = "unix_listen"
external shutdown : file_descr -> shutdown_command -> unit = "unix_shutdown"
external getsockname : file_descr -> sockaddr = "unix_getsockname"
external getpeername : file_descr -> sockaddr = "unix_getpeername"

external unsafe_recv :
  file_descr -> bytes -> int -> int -> msg_flag list -> int
                                  = "unix_recv"
external unsafe_recvfrom :
  file_descr -> bytes -> int -> int -> msg_flag list -> int * sockaddr
                                  = "unix_recvfrom"
external unsafe_send :
  file_descr -> bytes -> int -> int -> msg_flag list -> int
                                  = "unix_send"
external unsafe_sendto :
  file_descr -> bytes -> int -> int -> msg_flag list -> sockaddr -> int
                                  = "unix_sendto" "unix_sendto_native"

let recv fd buf ofs len flags =
  if ofs < 0 || len < 0 || ofs > Bytes.length buf - len
  then invalid_arg "Unix.recv"
  else unsafe_recv fd buf ofs len flags
let recvfrom fd buf ofs len flags =
  if ofs < 0 || len < 0 || ofs > Bytes.length buf - len
  then invalid_arg "Unix.recvfrom"
  else unsafe_recvfrom fd buf ofs len flags
let send fd buf ofs len flags =
  if ofs < 0 || len < 0 || ofs > Bytes.length buf - len
  then invalid_arg "Unix.send"
  else unsafe_send fd buf ofs len flags
let sendto fd buf ofs len flags addr =
  if ofs < 0 || len < 0 || ofs > Bytes.length buf - len
  then invalid_arg "Unix.sendto"
  else unsafe_sendto fd buf ofs len flags addr

let send_substring fd buf ofs len flags =
  send fd (Bytes.unsafe_of_string buf) ofs len flags

let sendto_substring fd buf ofs len flags addr =
  sendto fd (Bytes.unsafe_of_string buf) ofs len flags addr

type socket_bool_option =
    SO_DEBUG
  | SO_BROADCAST
  | SO_REUSEADDR
  | SO_KEEPALIVE
  | SO_DONTROUTE
  | SO_OOBINLINE
  | SO_ACCEPTCONN
  | TCP_NODELAY
  | IPV6_ONLY

type socket_int_option =
    SO_SNDBUF
  | SO_RCVBUF
  | SO_ERROR
  | SO_TYPE
  | SO_RCVLOWAT
  | SO_SNDLOWAT

type socket_optint_option = SO_LINGER

type socket_float_option =
    SO_RCVTIMEO
  | SO_SNDTIMEO

type socket_error_option = SO_ERROR

module SO: sig
  type ('opt, 'v) t
  val bool: (socket_bool_option, bool) t
  val int: (socket_int_option, int) t
  val optint: (socket_optint_option, int option) t
  val float: (socket_float_option, float) t
  val error: (socket_error_option, error option) t
  val get: ('opt, 'v) t -> file_descr -> 'opt -> 'v
  val set: ('opt, 'v) t -> file_descr -> 'opt -> 'v -> unit
end = struct
  type ('opt, 'v) t = int
  let bool = 0
  let int = 1
  let optint = 2
  let float = 3
  let error = 4
  external get: ('opt, 'v) t -> file_descr -> 'opt -> 'v
              = "unix_getsockopt"
  external set: ('opt, 'v) t -> file_descr -> 'opt -> 'v -> unit
              = "unix_setsockopt"
end

let getsockopt fd opt = SO.get SO.bool fd opt
let setsockopt fd opt v = SO.set SO.bool fd opt v

let getsockopt_int fd opt = SO.get SO.int fd opt
let setsockopt_int fd opt v = SO.set SO.int fd opt v

let getsockopt_optint fd opt = SO.get SO.optint fd opt
let setsockopt_optint fd opt v = SO.set SO.optint fd opt v

let getsockopt_float fd opt = SO.get SO.float fd opt
let setsockopt_float fd opt v = SO.set SO.float fd opt v

let getsockopt_error fd = SO.get SO.error fd SO_ERROR

(* Host and protocol databases *)

type host_entry =
  { h_name : string;
    h_aliases : string array;
    h_addrtype : socket_domain;
    h_addr_list : inet_addr array }

type protocol_entry =
  { p_name : string;
    p_aliases : string array;
    p_proto : int }

type service_entry =
  { s_name : string;
    s_aliases : string array;
    s_port : int;
    s_proto : string }

external gethostname : unit -> string = "unix_gethostname"
external gethostbyname : string -> host_entry = "unix_gethostbyname"
external gethostbyaddr : inet_addr -> host_entry = "unix_gethostbyaddr"
external getprotobyname : string -> protocol_entry
                                         = "unix_getprotobyname"
external getprotobynumber : int -> protocol_entry
                                         = "unix_getprotobynumber"

external getservbyname : string -> string -> service_entry
                                         = "unix_getservbyname"
external getservbyport : int -> string -> service_entry
                                         = "unix_getservbyport"

type addr_info =
  { ai_family : socket_domain;
    ai_socktype : socket_type;
    ai_protocol : int;
    ai_addr : sockaddr;
    ai_canonname : string }

type getaddrinfo_option =
    AI_FAMILY of socket_domain
  | AI_SOCKTYPE of socket_type
  | AI_PROTOCOL of int
  | AI_NUMERICHOST
  | AI_CANONNAME
  | AI_PASSIVE

external getaddrinfo_system
  : string -> string -> getaddrinfo_option list -> addr_info list
  = "unix_getaddrinfo"

let getaddrinfo_emulation node service opts =
  (* Parse options *)
  let opt_socktype = ref None
  and opt_protocol = ref 0
  and opt_passive = ref false in
  List.iter
    (function AI_SOCKTYPE s -> opt_socktype := Some s
            | AI_PROTOCOL p -> opt_protocol := p
            | AI_PASSIVE -> opt_passive := true
            | _ -> ())
    opts;
  (* Determine socket types and port numbers *)
  let get_port ty kind =
    if service = "" then [ty, 0] else
      try
        [ty, int_of_string service]
      with Failure _ ->
      try
        [ty, (getservbyname service kind).s_port]
      with Not_found -> []
  in
  let ports =
    match !opt_socktype with
    | None ->
        get_port SOCK_STREAM "tcp" @ get_port SOCK_DGRAM "udp"
    | Some SOCK_STREAM ->
        get_port SOCK_STREAM "tcp"
    | Some SOCK_DGRAM ->
        get_port SOCK_DGRAM "udp"
    | Some ty ->
        if service = "" then [ty, 0] else [] in
  (* Determine IP addresses *)
  let addresses =
    if node = "" then
      if List.mem AI_PASSIVE opts
      then [inet_addr_any, "0.0.0.0"]
      else [inet_addr_loopback, "127.0.0.1"]
    else
      try
        [inet_addr_of_string node, node]
      with Failure _ ->
      try
        let he = gethostbyname node in
        List.map
          (fun a -> (a, he.h_name))
          (Array.to_list he.h_addr_list)
      with Not_found ->
        [] in
  (* Cross-product of addresses and ports *)
  List.flatten
    (List.map
      (fun (ty, port) ->
        List.map
          (fun (addr, name) ->
            { ai_family = PF_INET;
              ai_socktype = ty;
              ai_protocol = !opt_protocol;
              ai_addr = ADDR_INET(addr, port);
              ai_canonname = name })
          addresses)
      ports)

let getaddrinfo node service opts =
  try
    List.rev(getaddrinfo_system node service opts)
  with Invalid_argument _ ->
    getaddrinfo_emulation node service opts

type name_info =
  { ni_hostname : string;
    ni_service : string }

type getnameinfo_option =
    NI_NOFQDN
  | NI_NUMERICHOST
  | NI_NAMEREQD
  | NI_NUMERICSERV
  | NI_DGRAM

external getnameinfo_system
  : sockaddr -> getnameinfo_option list -> name_info
  = "unix_getnameinfo"

let getnameinfo_emulation addr opts =
  match addr with
  | ADDR_UNIX f ->
      { ni_hostname = ""; ni_service = f } (* why not? *)
  | ADDR_INET(a, p) ->
      let hostname =
        try
          if List.mem NI_NUMERICHOST opts then raise Not_found;
          (gethostbyaddr a).h_name
        with Not_found ->
          if List.mem NI_NAMEREQD opts then raise Not_found;
          string_of_inet_addr a in
      let service =
        try
          if List.mem NI_NUMERICSERV opts then raise Not_found;
          let kind = if List.mem NI_DGRAM opts then "udp" else "tcp" in
          (getservbyport p kind).s_name
        with Not_found ->
          string_of_int p in
      { ni_hostname = hostname; ni_service = service }

let getnameinfo addr opts =
  try
    getnameinfo_system addr opts
  with Invalid_argument _ ->
    getnameinfo_emulation addr opts

(* High-level process management (system, popen) *)

external win_create_process : string -> string -> string option ->
                              file_descr -> file_descr -> file_descr -> int
                            = "win_create_process" "win_create_process_native"

let make_cmdline args =
  let maybe_quote f =
    if String.contains f ' ' ||
       String.contains f '\"' ||
       String.contains f '\t' ||
       f = ""
    then Filename.quote f
    else f in
  String.concat " " (List.map maybe_quote (Array.to_list args))

let make_process_env env =
  Array.iter
    (fun s -> if String.contains s '\000' then raise(Unix_error(EINVAL, "", s)))
    env;
  String.concat "\000" (Array.to_list env) ^ "\000"

let create_process prog args fd1 fd2 fd3 =
  win_create_process prog (make_cmdline args) None fd1 fd2 fd3

let create_process_env prog args env fd1 fd2 fd3 =
  win_create_process prog (make_cmdline args)
                     (Some(make_process_env env))
                     fd1 fd2 fd3

external system: string -> process_status = "win_system"

type popen_process =
    Process of in_channel * out_channel
  | Process_in of in_channel
  | Process_out of out_channel
  | Process_full of in_channel * out_channel * in_channel

let popen_processes = (Hashtbl.create 7 : (popen_process, int) Hashtbl.t)

let open_proc cmd optenv proc input output error =
  let shell =
    try Sys.getenv "COMSPEC"
    with Not_found -> raise(Unix_error(ENOEXEC, "open_proc", cmd)) in
  let pid =
    win_create_process shell (shell ^ " /c " ^ cmd) optenv
                       input output error in
  Hashtbl.add popen_processes proc pid

let open_process_in cmd =
  let (in_read, in_write) = pipe ~cloexec:true () in
  let inchan = in_channel_of_descr in_read in
  begin
    try
      open_proc cmd None (Process_in inchan) stdin in_write stderr
    with e ->
      close_in inchan;
      close in_write;
      raise e
  end;
  close in_write;
  inchan

let open_process_out cmd =
  let (out_read, out_write) = pipe ~cloexec:true () in
  let outchan = out_channel_of_descr out_write in
  begin
    try
      open_proc cmd None (Process_out outchan) out_read stdout stderr
    with e ->
    close_out outchan;
    close out_read;
    raise e
  end;
  close out_read;
  outchan

let open_process cmd =
  let (in_read, in_write) = pipe ~cloexec:true () in
  let (out_read, out_write) =
    try pipe ~cloexec:true ()
    with e -> close in_read; close in_write; raise e in
  let inchan = in_channel_of_descr in_read in
  let outchan = out_channel_of_descr out_write in
  begin
    try
      open_proc cmd None (Process(inchan, outchan)) out_read in_write stderr
    with e ->
      close out_read; close out_write;
      close in_read; close in_write;
      raise e
  end;
  close out_read;
  close in_write;
  (inchan, outchan)

let open_process_full cmd env =
  let (in_read, in_write) = pipe ~cloexec:true () in
  let (out_read, out_write) =
    try pipe ~cloexec:true ()
    with e -> close in_read; close in_write; raise e in
  let (err_read, err_write) =
    try pipe ~cloexec:true ()
    with e -> close in_read; close in_write;
              close out_read; close out_write; raise e in
  let inchan = in_channel_of_descr in_read in
  let outchan = out_channel_of_descr out_write in
  let errchan = in_channel_of_descr err_read in
  begin
    try
      open_proc cmd (Some (make_process_env env))
               (Process_full(inchan, outchan, errchan))
                out_read in_write err_write
    with e ->
      close out_read; close out_write;
      close in_read; close in_write;
      close err_read; close err_write;
      raise e
  end;
  close out_read;
  close in_write;
  close err_write;
  (inchan, outchan, errchan)

let find_proc_id fun_name proc =
  try
    let pid = Hashtbl.find popen_processes proc in
    Hashtbl.remove popen_processes proc;
    pid
  with Not_found ->
    raise(Unix_error(EBADF, fun_name, ""))

let close_process_in inchan =
  let pid = find_proc_id "close_process_in" (Process_in inchan) in
  close_in inchan;
  snd(waitpid [] pid)

let close_process_out outchan =
  let pid = find_proc_id "close_process_out" (Process_out outchan) in
  close_out outchan;
  snd(waitpid [] pid)

let close_process (inchan, outchan) =
  let pid = find_proc_id "close_process" (Process(inchan, outchan)) in
  close_in inchan; close_out outchan;
  snd(waitpid [] pid)

let close_process_full (inchan, outchan, errchan) =
  let pid =
    find_proc_id "close_process_full"
                 (Process_full(inchan, outchan, errchan)) in
  close_in inchan; close_out outchan; close_in errchan;
  snd(waitpid [] pid)

(* Polling *)

external select :
  file_descr list -> file_descr list -> file_descr list -> float ->
        file_descr list * file_descr list * file_descr list = "unix_select"

(* High-level network functions *)

let open_connection sockaddr =
  let sock =
    socket ~cloexec:true (domain_of_sockaddr sockaddr) SOCK_STREAM 0 in
  try
    connect sock sockaddr;
    (in_channel_of_descr sock, out_channel_of_descr sock)
  with exn ->
    close sock; raise exn

let shutdown_connection inchan =
  shutdown (descr_of_in_channel inchan) SHUTDOWN_SEND

let establish_server _server_fun _sockaddr =
  invalid_arg "Unix.establish_server not implemented"

(* Terminal interface *)

type terminal_io = {
    mutable c_ignbrk: bool;
    mutable c_brkint: bool;
    mutable c_ignpar: bool;
    mutable c_parmrk: bool;
    mutable c_inpck: bool;
    mutable c_istrip: bool;
    mutable c_inlcr: bool;
    mutable c_igncr: bool;
    mutable c_icrnl: bool;
    mutable c_ixon: bool;
    mutable c_ixoff: bool;
    mutable c_opost: bool;
    mutable c_obaud: int;
    mutable c_ibaud: int;
    mutable c_csize: int;
    mutable c_cstopb: int;
    mutable c_cread: bool;
    mutable c_parenb: bool;
    mutable c_parodd: bool;
    mutable c_hupcl: bool;
    mutable c_clocal: bool;
    mutable c_isig: bool;
    mutable c_icanon: bool;
    mutable c_noflsh: bool;
    mutable c_echo: bool;
    mutable c_echoe: bool;
    mutable c_echok: bool;
    mutable c_echonl: bool;
    mutable c_vintr: char;
    mutable c_vquit: char;
    mutable c_verase: char;
    mutable c_vkill: char;
    mutable c_veof: char;
    mutable c_veol: char;
    mutable c_vmin: int;
    mutable c_vtime: int;
    mutable c_vstart: char;
    mutable c_vstop: char
  }

type setattr_when = TCSANOW | TCSADRAIN | TCSAFLUSH

let tcgetattr _fd = invalid_arg "Unix.tcgetattr not implemented"
let tcsetattr _fd _wh = invalid_arg "Unix.tcsetattr not implemented"
let tcsendbreak _fd _n = invalid_arg "Unix.tcsendbreak not implemented"
let tcdrain _fd = invalid_arg "Unix.tcdrain not implemented"

type flush_queue = TCIFLUSH | TCOFLUSH | TCIOFLUSH
let tcflush _fd _q = invalid_arg "Unix.tcflush not implemented"
type flow_action = TCOOFF | TCOON | TCIOFF | TCION
let tcflow _fd _fl = invalid_arg "Unix.tcflow not implemented"
let setsid () = invalid_arg "Unix.setsid not implemented"
