(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Interface to the Unix system.

    Note: all the functions of this module (except {!error_message} and
    {!handle_unix_error}) are liable to raise the {!Unix_error}
    exception whenever the underlying system call signals an error. *)


(** {1 Error report} *)


type error =
    E2BIG               (** Argument list too long *)
  | EACCES              (** Permission denied *)
  | EAGAIN              (** Resource temporarily unavailable; try again *)
  | EBADF               (** Bad file descriptor *)
  | EBUSY               (** Resource unavailable *)
  | ECHILD              (** No child process *)
  | EDEADLK             (** Resource deadlock would occur *)
  | EDOM                (** Domain error for math functions, etc. *)
  | EEXIST              (** File exists *)
  | EFAULT              (** Bad address *)
  | EFBIG               (** File too large *)
  | EINTR               (** Function interrupted by signal *)
  | EINVAL              (** Invalid argument *)
  | EIO                 (** Hardware I/O error *)
  | EISDIR              (** Is a directory *)
  | EMFILE              (** Too many open files by the process *)
  | EMLINK              (** Too many links *)
  | ENAMETOOLONG        (** Filename too long *)
  | ENFILE              (** Too many open files in the system *)
  | ENODEV              (** No such device *)
  | ENOENT              (** No such file or directory *)
  | ENOEXEC             (** Not an executable file *)
  | ENOLCK              (** No locks available *)
  | ENOMEM              (** Not enough memory *)
  | ENOSPC              (** No space left on device *)
  | ENOSYS              (** Function not supported *)
  | ENOTDIR             (** Not a directory *)
  | ENOTEMPTY           (** Directory not empty *)
  | ENOTTY              (** Inappropriate I/O control operation *)
  | ENXIO               (** No such device or address *)
  | EPERM               (** Operation not permitted *)
  | EPIPE               (** Broken pipe *)
  | ERANGE              (** Result too large *)
  | EROFS               (** Read-only file system *)
  | ESPIPE              (** Invalid seek e.g. on a pipe *)
  | ESRCH               (** No such process *)
  | EXDEV               (** Invalid link *)
  | EWOULDBLOCK         (** Operation would block *)
  | EINPROGRESS         (** Operation now in progress *)
  | EALREADY            (** Operation already in progress *)
  | ENOTSOCK            (** Socket operation on non-socket *)
  | EDESTADDRREQ        (** Destination address required *)
  | EMSGSIZE            (** Message too long *)
  | EPROTOTYPE          (** Protocol wrong type for socket *)
  | ENOPROTOOPT         (** Protocol not available *)
  | EPROTONOSUPPORT     (** Protocol not supported *)
  | ESOCKTNOSUPPORT     (** Socket type not supported *)
  | EOPNOTSUPP          (** Operation not supported on socket *)
  | EPFNOSUPPORT        (** Protocol family not supported *)
  | EAFNOSUPPORT        (** Address family not supported by protocol family *)
  | EADDRINUSE          (** Address already in use *)
  | EADDRNOTAVAIL       (** Can't assign requested address *)
  | ENETDOWN            (** Network is down *)
  | ENETUNREACH         (** Network is unreachable *)
  | ENETRESET           (** Network dropped connection on reset *)
  | ECONNABORTED        (** Software caused connection abort *)
  | ECONNRESET          (** Connection reset by peer *)
  | ENOBUFS             (** No buffer space available *)
  | EISCONN             (** Socket is already connected *)
  | ENOTCONN            (** Socket is not connected *)
  | ESHUTDOWN           (** Can't send after socket shutdown *)
  | ETOOMANYREFS        (** Too many references: can't splice *)
  | ETIMEDOUT           (** Connection timed out *)
  | ECONNREFUSED        (** Connection refused *)
  | EHOSTDOWN           (** Host is down *)
  | EHOSTUNREACH        (** No route to host *)
  | ELOOP               (** Too many levels of symbolic links *)
  | EOVERFLOW           (** File size or position not representable *)

  | EUNKNOWNERR of int  (** Unknown error *)
(** The type of error codes.
   Errors defined in the POSIX standard
   and additional errors from UNIX98 and BSD.
   All other errors are mapped to EUNKNOWNERR.
*)


exception Unix_error of error * string * string
(** Raised by the system calls below when an error is encountered.
   The first component is the error code; the second component
   is the function name; the third component is the string parameter
   to the function, if it has one, or the empty string otherwise. *)

val error_message : error -> string
(** Return a string describing the given error code. *)

val handle_unix_error : ('a -> 'b) -> 'a -> 'b
(** [handle_unix_error f x] applies [f] to [x] and returns the result.
   If the exception {!Unix_error} is raised, it prints a message
   describing the error and exits with code 2. *)


(** {1 Access to the process environment} *)


val environment : unit -> string array
(** Return the process environment, as an array of strings
    with the format ``variable=value''.  The returned array
    is empty if the process has special privileges. *)

val unsafe_environment : unit -> string array
(** Return the process environment, as an array of strings with the
    format ``variable=value''.  Unlike {!environment}, this function
    returns a populated array even if the process has special
    privileges.  See the documentation for {!unsafe_getenv} for more
    details.

    @since 4.06.0 *)

val getenv : string -> string
(** Return the value associated to a variable in the process
   environment, unless the process has special privileges.
   @raise Not_found if the variable is unbound or the process has
   special privileges.

   (This function is identical to {!Sys.getenv}. *)

val unsafe_getenv : string -> string
(** Return the value associated to a variable in the process
   environment.

   Unlike {!getenv}, this function returns the value even if the
   process has special privileges. It is considered unsafe because the
   programmer of a setuid or setgid program must be careful to avoid
   using maliciously crafted environment variables in the search path
   for executables, the locations for temporary files or logs, and the
   like.

   @raise Not_found if the variable is unbound.
   @since 4.06.0  *)

val putenv : string -> string -> unit
(** [Unix.putenv name value] sets the value associated to a
   variable in the process environment.
   [name] is the name of the environment variable,
   and [value] its new associated value. *)


(** {1 Process handling} *)


type process_status =
    WEXITED of int
        (** The process terminated normally by [exit];
           the argument is the return code. *)
  | WSIGNALED of int
        (** The process was killed by a signal;
           the argument is the signal number. *)
  | WSTOPPED of int
        (** The process was stopped by a signal; the argument is the
           signal number. *)
(** The termination status of a process.  See module {!Sys} for the
    definitions of the standard signal numbers.  Note that they are
    not the numbers used by the OS. *)


type wait_flag =
    WNOHANG (** Do not block if no child has
               died yet, but immediately return with a pid equal to 0.*)
  | WUNTRACED (** Report also the children that receive stop signals. *)
(** Flags for {!Unix.waitpid}. *)

val execv : string -> string array -> 'a
(** [execv prog args] execute the program in file [prog], with
   the arguments [args], and the current process environment.
   These [execv*] functions never return: on success, the current
   program is replaced by the new one.
   @raise Unix.Unix_error on failure. *)

val execve : string -> string array -> string array -> 'a
(** Same as {!Unix.execv}, except that the third argument provides the
   environment to the program executed. *)

val execvp : string -> string array -> 'a
(** Same as {!Unix.execv}, except that
   the program is searched in the path. *)

val execvpe : string -> string array -> string array -> 'a
(** Same as {!Unix.execve}, except that
   the program is searched in the path. *)

val fork : unit -> int
(** Fork a new process. The returned integer is 0 for the child
   process, the pid of the child process for the parent process.

   On Windows: not implemented, use {!create_process} or threads. *)

val wait : unit -> int * process_status
(** Wait until one of the children processes die, and return its pid
   and termination status.

   On Windows: Not implemented, use {!waitpid}. *)

val waitpid : wait_flag list -> int -> int * process_status
(** Same as {!Unix.wait}, but waits for the child process whose pid is given.
   A pid of [-1] means wait for any child.
   A pid of [0] means wait for any child in the same process group
   as the current process.
   Negative pid arguments represent process groups.
   The list of options indicates whether [waitpid] should return
   immediately without waiting, and whether it should report stopped
   children.

   On Windows, this function can only wait for a given PID, not any
   child process. *)

val system : string -> process_status
(** Execute the given command, wait until it terminates, and return
   its termination status. The string is interpreted by the shell
   [/bin/sh] (or the command interpreter [cmd.exe] on Windows) and
   therefore can contain redirections, quotes, variables, etc. The
   result [WEXITED 127] indicates that the shell couldn't be
   executed. *)

val getpid : unit -> int
(** Return the pid of the process. *)

val getppid : unit -> int
(** Return the pid of the parent process.
   On Windows: not implemented (because it is meaningless). *)

val nice : int -> int
(** Change the process priority. The integer argument is added to the
   ``nice'' value. (Higher values of the ``nice'' value mean
   lower priorities.) Return the new nice value.

   On Windows: not implemented. *)


(** {1 Basic file input/output} *)


type file_descr
(** The abstract type of file descriptors. *)

val stdin : file_descr
(** File descriptor for standard input.*)

val stdout : file_descr
(** File descriptor for standard output.*)

val stderr : file_descr
(** File descriptor for standard error. *)

type open_flag =
    O_RDONLY                    (** Open for reading *)
  | O_WRONLY                    (** Open for writing *)
  | O_RDWR                      (** Open for reading and writing *)
  | O_NONBLOCK                  (** Open in non-blocking mode *)
  | O_APPEND                    (** Open for append *)
  | O_CREAT                     (** Create if nonexistent *)
  | O_TRUNC                     (** Truncate to 0 length if existing *)
  | O_EXCL                      (** Fail if existing *)
  | O_NOCTTY                    (** Don't make this dev a controlling tty *)
  | O_DSYNC                     (** Writes complete as `Synchronised I/O data
                                   integrity completion' *)
  | O_SYNC                      (** Writes complete as `Synchronised I/O file
                                   integrity completion' *)
  | O_RSYNC                     (** Reads complete as writes (depending on
                                   O_SYNC/O_DSYNC) *)
  | O_SHARE_DELETE              (** Windows only: allow the file to be deleted
                                   while still open *)
  | O_CLOEXEC                   (** Set the close-on-exec flag on the
                                   descriptor returned by {!openfile}.
                                   See {!set_close_on_exec} for more
                                   information. *)
  | O_KEEPEXEC                  (** Clear the close-on-exec flag.
                                    This is currently the default. *)
(** The flags to {!Unix.openfile}. *)


type file_perm = int
(** The type of file access rights, e.g. [0o640] is read and write for user,
    read for group, none for others *)

val openfile : string -> open_flag list -> file_perm -> file_descr
(** Open the named file with the given flags. Third argument is the
   permissions to give to the file if it is created (see
   {!umask}). Return a file descriptor on the named file. *)

val close : file_descr -> unit
(** Close a file descriptor. *)

val read : file_descr -> bytes -> int -> int -> int
(** [read fd buff ofs len] reads [len] bytes from descriptor [fd],
    storing them in byte sequence [buff], starting at position [ofs] in
    [buff]. Return the number of bytes actually read. *)

val write : file_descr -> bytes -> int -> int -> int
(** [write fd buff ofs len] writes [len] bytes to descriptor [fd],
    taking them from byte sequence [buff], starting at position [ofs]
    in [buff]. Return the number of bytes actually written.  [write]
    repeats the writing operation until all bytes have been written or
    an error occurs.  *)

val single_write : file_descr -> bytes -> int -> int -> int
(** Same as [write], but attempts to write only once.
   Thus, if an error occurs, [single_write] guarantees that no data
   has been written. *)

val write_substring : file_descr -> string -> int -> int -> int
(** Same as [write], but take the data from a string instead of a byte
    sequence.
    @since 4.02.0 *)

val single_write_substring : file_descr -> string -> int -> int -> int
(** Same as [single_write], but take the data from a string instead of
    a byte sequence.
    @since 4.02.0 *)

(** {1 Interfacing with the standard input/output library} *)



val in_channel_of_descr : file_descr -> in_channel
(** Create an input channel reading from the given descriptor.
   The channel is initially in binary mode; use
   [set_binary_mode_in ic false] if text mode is desired.
   Text mode is supported only if the descriptor refers to a file
   or pipe, but is not supported if it refers to a socket.
   On Windows, [set_binary_mode_in] always fails on channels created
   with this function.

   Beware that channels are buffered so more characters may have been
   read from the file descriptor than those accessed using channel functions.
   Channels also keep a copy of the current position in the file.

   You need to explicitly close all channels created with this function.
   Closing the channel also closes the underlying file descriptor (unless
   it was already closed). *)

val out_channel_of_descr : file_descr -> out_channel
(** Create an output channel writing on the given descriptor.
   The channel is initially in binary mode; use
   [set_binary_mode_out oc false] if text mode is desired.
   Text mode is supported only if the descriptor refers to a file
   or pipe, but is not supported if it refers to a socket.
   On Windows, [set_binary_mode_out] always fails on channels created
   with this function.

   Beware that channels are buffered so you may have to [flush] them
   to ensure that all data has been sent to the file descriptor.
   Channels also keep a copy of the current position in the file.

   You need to explicitly close all channels created with this function.
   Closing the channel flushes the data and closes the underlying file
   descriptor (unless it has already been closed, in which case the
   buffered data is lost).*)

val descr_of_in_channel : in_channel -> file_descr
(** Return the descriptor corresponding to an input channel. *)

val descr_of_out_channel : out_channel -> file_descr
(** Return the descriptor corresponding to an output channel. *)


(** {1 Seeking and truncating} *)


type seek_command =
    SEEK_SET (** indicates positions relative to the beginning of the file *)
  | SEEK_CUR (** indicates positions relative to the current position *)
  | SEEK_END (** indicates positions relative to the end of the file *)
(** Positioning modes for {!Unix.lseek}. *)


val lseek : file_descr -> int -> seek_command -> int
(** Set the current position for a file descriptor, and return the resulting
    offset (from the beginning of the file). *)

val truncate : string -> int -> unit
(** Truncates the named file to the given size.

  On Windows: not implemented. *)

val ftruncate : file_descr -> int -> unit
(** Truncates the file corresponding to the given descriptor
   to the given size.

  On Windows: not implemented. *)


(** {1 File status} *)


type file_kind =
    S_REG                       (** Regular file *)
  | S_DIR                       (** Directory *)
  | S_CHR                       (** Character device *)
  | S_BLK                       (** Block device *)
  | S_LNK                       (** Symbolic link *)
  | S_FIFO                      (** Named pipe *)
  | S_SOCK                      (** Socket *)

type stats =
  { st_dev : int;               (** Device number *)
    st_ino : int;               (** Inode number *)
    st_kind : file_kind;        (** Kind of the file *)
    st_perm : file_perm;        (** Access rights *)
    st_nlink : int;             (** Number of links *)
    st_uid : int;               (** User id of the owner *)
    st_gid : int;               (** Group ID of the file's group *)
    st_rdev : int;              (** Device minor number *)
    st_size : int;              (** Size in bytes *)
    st_atime : float;           (** Last access time *)
    st_mtime : float;           (** Last modification time *)
    st_ctime : float;           (** Last status change time *)
  }
(** The information returned by the {!Unix.stat} calls. *)

val stat : string -> stats
(** Return the information for the named file. *)

val lstat : string -> stats
(** Same as {!Unix.stat}, but in case the file is a symbolic link,
   return the information for the link itself. *)

val fstat : file_descr -> stats
(** Return the information for the file associated with the given
   descriptor. *)

val isatty : file_descr -> bool
(** Return [true] if the given file descriptor refers to a terminal or
   console window, [false] otherwise. *)

(** {1 File operations on large files} *)

module LargeFile :
  sig
    val lseek : file_descr -> int64 -> seek_command -> int64
    (** See {!Unix.lseek}. *)

    val truncate : string -> int64 -> unit
    (** See {!Unix.truncate}. *)

    val ftruncate : file_descr -> int64 -> unit
    (** See {!Unix.ftruncate}. *)

    type stats =
      { st_dev : int;               (** Device number *)
        st_ino : int;               (** Inode number *)
        st_kind : file_kind;        (** Kind of the file *)
        st_perm : file_perm;        (** Access rights *)
        st_nlink : int;             (** Number of links *)
        st_uid : int;               (** User id of the owner *)
        st_gid : int;               (** Group ID of the file's group *)
        st_rdev : int;              (** Device minor number *)
        st_size : int64;            (** Size in bytes *)
        st_atime : float;           (** Last access time *)
        st_mtime : float;           (** Last modification time *)
        st_ctime : float;           (** Last status change time *)
      }
    val stat : string -> stats
    val lstat : string -> stats
    val fstat : file_descr -> stats
  end
(** File operations on large files.
  This sub-module provides 64-bit variants of the functions
  {!Unix.lseek} (for positioning a file descriptor),
  {!Unix.truncate} and {!Unix.ftruncate} (for changing the size of a file),
  and {!Unix.stat}, {!Unix.lstat} and {!Unix.fstat} (for obtaining
  information on files).  These alternate functions represent
  positions and sizes by 64-bit integers (type [int64]) instead of
  regular integers (type [int]), thus allowing operating on files
  whose sizes are greater than [max_int]. *)

(** {6 Mapping files into memory} *)

val map_file :
  file_descr -> ?pos:int64 -> ('a, 'b) CamlinternalBigarray.kind ->
  'c CamlinternalBigarray.layout -> bool -> int array ->
  ('a, 'b, 'c) CamlinternalBigarray.genarray
(** Memory mapping of a file as a big array.
  [map_file fd kind layout shared dims]
  returns a big array of kind [kind], layout [layout],
  and dimensions as specified in [dims].  The data contained in
  this big array are the contents of the file referred to by
  the file descriptor [fd] (as opened previously with
  [Unix.openfile], for example).  The optional [pos] parameter
  is the byte offset in the file of the data being mapped;
  it defaults to 0 (map from the beginning of the file).

  If [shared] is [true], all modifications performed on the array
  are reflected in the file.  This requires that [fd] be opened
  with write permissions.  If [shared] is [false], modifications
  performed on the array are done in memory only, using
  copy-on-write of the modified pages; the underlying file is not
  affected.

  [Genarray.map_file] is much more efficient than reading
  the whole file in a big array, modifying that big array,
  and writing it afterwards.

  To adjust automatically the dimensions of the big array to
  the actual size of the file, the major dimension (that is,
  the first dimension for an array with C layout, and the last
  dimension for an array with Fortran layout) can be given as
  [-1].  [Genarray.map_file] then determines the major dimension
  from the size of the file.  The file must contain an integral
  number of sub-arrays as determined by the non-major dimensions,
  otherwise [Failure] is raised.

  If all dimensions of the big array are given, the file size is
  matched against the size of the big array.  If the file is larger
  than the big array, only the initial portion of the file is
  mapped to the big array.  If the file is smaller than the big
  array, the file is automatically grown to the size of the big array.
  This requires write permissions on [fd].

  Array accesses are bounds-checked, but the bounds are determined by
  the initial call to [map_file]. Therefore, you should make sure no
  other process modifies the mapped file while you're accessing it,
  or a SIGBUS signal may be raised. This happens, for instance, if the
  file is shrunk.

  [Invalid_argument] or [Failure] may be raised in cases where argument
  validation fails.
  @since 4.06.0 *)

(** {1 Operations on file names} *)


val unlink : string -> unit
(** Removes the named file.

    If the named file is a directory, raises:
    {ul
    {- [EPERM] on POSIX compliant system}
    {- [EISDIR] on Linux >= 2.1.132}
    {- [EACCESS] on Windows}}
*)

val rename : string -> string -> unit
(** [rename old new] changes the name of a file from [old] to [new],
    moving it between directories if needed.  If [new] already
    exists, its contents will be replaced with those of [old].
    Depending on the operating system, the metadata (permissions,
    owner, etc) of [new] can either be preserved or be replaced by
    those of [old].  *)

val link : string -> string -> unit
(** [link source dest] creates a hard link named [dest] to the file
   named [source]. *)


(** {1 File permissions and ownership} *)


type access_permission =
    R_OK                        (** Read permission *)
  | W_OK                        (** Write permission *)
  | X_OK                        (** Execution permission *)
  | F_OK                        (** File exists *)
(** Flags for the {!Unix.access} call. *)


val chmod : string -> file_perm -> unit
(** Change the permissions of the named file. *)

val fchmod : file_descr -> file_perm -> unit
(** Change the permissions of an opened file.
    On Windows: not implemented. *)

val chown : string -> int -> int -> unit
(** Change the owner uid and owner gid of the named file.
    On Windows: not implemented (make no sense on a DOS file system). *)

val fchown : file_descr -> int -> int -> unit
(** Change the owner uid and owner gid of an opened file.
    On Windows: not implemented (make no sense on a DOS file system).  *)

val umask : int -> int
(** Set the process's file mode creation mask, and return the previous
    mask.
    On Windows: not implemented. *)

val access : string -> access_permission list -> unit
(** Check that the process has the given permissions over the named file.
   @raise Unix_error otherwise.

   On Windows, execute permission [X_OK], cannot be tested, it just
   tests for read permission instead. *)


(** {1 Operations on file descriptors} *)


val dup : ?cloexec:bool -> file_descr -> file_descr
(** Return a new file descriptor referencing the same file as
   the given descriptor.
   See {!set_close_on_exec} for documentation on the [cloexec]
   optional argument. *)

val dup2 : ?cloexec:bool -> file_descr -> file_descr -> unit
(** [dup2 fd1 fd2] duplicates [fd1] to [fd2], closing [fd2] if already
   opened.
   See {!set_close_on_exec} for documentation on the [cloexec]
   optional argument. *)

val set_nonblock : file_descr -> unit
(** Set the ``non-blocking'' flag on the given descriptor.
   When the non-blocking flag is set, reading on a descriptor
   on which there is temporarily no data available raises the
   [EAGAIN] or [EWOULDBLOCK] error instead of blocking;
   writing on a descriptor on which there is temporarily no room
   for writing also raises [EAGAIN] or [EWOULDBLOCK]. *)

val clear_nonblock : file_descr -> unit
(** Clear the ``non-blocking'' flag on the given descriptor.
   See {!Unix.set_nonblock}.*)

val set_close_on_exec : file_descr -> unit
(** Set the ``close-on-exec'' flag on the given descriptor.
   A descriptor with the close-on-exec flag is automatically
   closed when the current process starts another program with
   one of the [exec], [create_process] and [open_process] functions.

   It is often a security hole to leak file descriptors opened on, say,
   a private file to an external program: the program, then, gets access
   to the private file and can do bad things with it.  Hence, it is
   highly recommended to set all file descriptors ``close-on-exec'',
   except in the very few cases where a file descriptor actually needs
   to be transmitted to another program.  

   The best way to set a file descriptor ``close-on-exec'' is to create
   it in this state.  To this end, the [openfile] function has
   [O_CLOEXEC] and [O_KEEPEXEC] flags to enforce ``close-on-exec'' mode
   or ``keep-on-exec'' mode, respectively.  All other operations in
   the Unix module that create file descriptors have an optional
   argument [?cloexec:bool] to indicate whether the file descriptor
   should be created in ``close-on-exec'' mode (by writing
   [~cloexec:true]) or in ``keep-on-exec'' mode (by writing
   [~cloexec:false]).  For historical reasons, the default file
   descriptor creation mode is ``keep-on-exec'', if no [cloexec] optional
   argument is given.  This is not a safe default, hence it is highly
   recommended to pass explicit [cloexec] arguments to operations that
   create file descriptors.

   The [cloexec] optional arguments and the [O_KEEPEXEC] flag were introduced
   in OCaml 4.05.  Earlier, the common practice was to create file descriptors
   in the default, ``keep-on-exec'' mode, then call [set_close_on_exec]
   on those freshly-created file descriptors.  This is not as safe as
   creating the file descriptor in ``close-on-exec'' mode because, in
   multithreaded programs, a window of vulnerability exists between the time
   when the file descriptor is created and the time [set_close_on_exec]
   completes.  If another thread spawns another program during this window,
   the descriptor will leak, as it is still in the ``keep-on-exec'' mode.

   Regarding the atomicity guarantees given by [~cloexec:true] or by
   the use of the [O_CLOEXEC] flag: on all platforms it is guaranteed
   that a concurrently-executing Caml thread cannot leak the descriptor
   by starting a new process.  On Linux, this guarantee extends to
   concurrently-executing C threads.  As of Feb 2017, other operating
   systems lack the necessary system calls and still expose a window
   of vulnerability during which a C thread can see the newly-created
   file descriptor in ``keep-on-exec'' mode.
 *)

val clear_close_on_exec : file_descr -> unit
(** Clear the ``close-on-exec'' flag on the given descriptor.
   See {!Unix.set_close_on_exec}.*)


(** {1 Directories} *)


val mkdir : string -> file_perm -> unit
(** Create a directory with the given permissions (see {!umask}). *)

val rmdir : string -> unit
(** Remove an empty directory. *)

val chdir : string -> unit
(** Change the process working directory. *)

val getcwd : unit -> string
(** Return the name of the current working directory. *)

val chroot : string -> unit
(** Change the process root directory.
    On Windows: not implemented. *)

type dir_handle
(** The type of descriptors over opened directories. *)

val opendir : string -> dir_handle
(** Open a descriptor on a directory *)

val readdir : dir_handle -> string
(** Return the next entry in a directory.
   @raise End_of_file when the end of the directory has been reached. *)

val rewinddir : dir_handle -> unit
(** Reposition the descriptor to the beginning of the directory *)

val closedir : dir_handle -> unit
(** Close a directory descriptor. *)



(** {1 Pipes and redirections} *)


val pipe : ?cloexec:bool -> unit -> file_descr * file_descr
(** Create a pipe. The first component of the result is opened
   for reading, that's the exit to the pipe. The second component is
   opened for writing, that's the entrance to the pipe.
   See {!set_close_on_exec} for documentation on the [cloexec]
   optional argument. *)

val mkfifo : string -> file_perm -> unit
(** Create a named pipe with the given permissions (see {!umask}).
   On Windows: not implemented. *)


(** {1 High-level process and redirection management} *)


val create_process :
  string -> string array -> file_descr -> file_descr -> file_descr -> int
(** [create_process prog args new_stdin new_stdout new_stderr]
   forks a new process that executes the program
   in file [prog], with arguments [args]. The pid of the new
   process is returned immediately; the new process executes
   concurrently with the current process.
   The standard input and outputs of the new process are connected
   to the descriptors [new_stdin], [new_stdout] and [new_stderr].
   Passing e.g. [stdout] for [new_stdout] prevents the redirection
   and causes the new process to have the same standard output
   as the current process.
   The executable file [prog] is searched in the path.
   The new process has the same environment as the current process. *)

val create_process_env :
  string -> string array -> string array -> file_descr -> file_descr ->
    file_descr -> int
(** [create_process_env prog args env new_stdin new_stdout new_stderr]
   works as {!Unix.create_process}, except that the extra argument
   [env] specifies the environment passed to the program. *)


val open_process_in : string -> in_channel
(** High-level pipe and process management. This function
   runs the given command in parallel with the program.
   The standard output of the command is redirected to a pipe,
   which can be read via the returned input channel.
   The command is interpreted by the shell [/bin/sh]
   (or [cmd.exe] on Windows), cf. [system]. *)

val open_process_out : string -> out_channel
(** Same as {!Unix.open_process_in}, but redirect the standard input of
   the command to a pipe.  Data written to the returned output channel
   is sent to the standard input of the command.
   Warning: writes on output channels are buffered, hence be careful
   to call {!Pervasives.flush} at the right times to ensure
   correct synchronization. *)

val open_process : string -> in_channel * out_channel
(** Same as {!Unix.open_process_out}, but redirects both the standard input
   and standard output of the command to pipes connected to the two
   returned channels.  The input channel is connected to the output
   of the command, and the output channel to the input of the command. *)

val open_process_full :
  string -> string array -> in_channel * out_channel * in_channel
(** Similar to {!Unix.open_process}, but the second argument specifies
   the environment passed to the command.  The result is a triple
   of channels connected respectively to the standard output, standard input,
   and standard error of the command. *)

val close_process_in : in_channel -> process_status
(** Close channels opened by {!Unix.open_process_in},
   wait for the associated command to terminate,
   and return its termination status. *)

val close_process_out : out_channel -> process_status
(** Close channels opened by {!Unix.open_process_out},
   wait for the associated command to terminate,
   and return its termination status. *)

val close_process : in_channel * out_channel -> process_status
(** Close channels opened by {!Unix.open_process},
   wait for the associated command to terminate,
   and return its termination status. *)

val close_process_full :
  in_channel * out_channel * in_channel -> process_status
(** Close channels opened by {!Unix.open_process_full},
   wait for the associated command to terminate,
   and return its termination status. *)


(** {1 Symbolic links} *)


val symlink : ?to_dir:bool -> string -> string -> unit
(** [symlink ?to_dir source dest] creates the file [dest] as a symbolic link
   to the file [source]. On Windows, [~to_dir] indicates if the symbolic link
   points to a directory or a file; if omitted, [symlink] examines [source]
   using [stat] and picks appropriately, if [source] does not exist then [false]
   is assumed (for this reason, it is recommended that the [~to_dir] parameter
   be specified in new code). On Unix, [~to_dir] is ignored.

   Windows symbolic links are available in Windows Vista onwards. There are some
   important differences between Windows symlinks and their POSIX counterparts.

   Windows symbolic links come in two flavours: directory and regular, which
   designate whether the symbolic link points to a directory or a file. The type
   must be correct - a directory symlink which actually points to a file cannot
   be selected with chdir and a file symlink which actually points to a
   directory cannot be read or written (note that Cygwin's emulation layer
   ignores this distinction).

   When symbolic links are created to existing targets, this distinction doesn't
   matter and [symlink] will automatically create the correct kind of symbolic
   link. The distinction matters when a symbolic link is created to a
   non-existent target.

   The other caveat is that by default symbolic links are a privileged
   operation. Administrators will always need to be running elevated (or with
   UAC disabled) and by default normal user accounts need to be granted the
   SeCreateSymbolicLinkPrivilege via Local Security Policy (secpol.msc) or via
   Active Directory.

   {!has_symlink} can be used to check that a process is able to create symbolic
   links. *)

val has_symlink : unit -> bool
(** Returns [true] if the user is able to create symbolic links. On Windows,
   this indicates that the user not only has the SeCreateSymbolicLinkPrivilege
   but is also running elevated, if necessary. On other platforms, this is
   simply indicates that the symlink system call is available.
   @since 4.03.0 *)

val readlink : string -> string
(** Read the contents of a symbolic link. *)


(** {1 Polling} *)


val select :
  file_descr list -> file_descr list -> file_descr list -> float ->
    file_descr list * file_descr list * file_descr list
(** Wait until some input/output operations become possible on
   some channels. The three list arguments are, respectively, a set
   of descriptors to check for reading (first argument), for writing
   (second argument), or for exceptional conditions (third argument).
   The fourth argument is the maximal timeout, in seconds; a
   negative fourth argument means no timeout (unbounded wait).
   The result is composed of three sets of descriptors: those ready
   for reading (first component), ready for writing (second component),
   and over which an exceptional condition is pending (third
   component). *)


(** {1 Locking} *)

type lock_command =
    F_ULOCK       (** Unlock a region *)
  | F_LOCK        (** Lock a region for writing, and block if already locked *)
  | F_TLOCK       (** Lock a region for writing, or fail if already locked *)
  | F_TEST        (** Test a region for other process locks *)
  | F_RLOCK       (** Lock a region for reading, and block if already locked *)
  | F_TRLOCK      (** Lock a region for reading, or fail if already locked *)
(** Commands for {!Unix.lockf}. *)

val lockf : file_descr -> lock_command -> int -> unit
(** [lockf fd cmd size] puts a lock on a region of the file opened
   as [fd]. The region starts at the current read/write position for
   [fd] (as set by {!Unix.lseek}), and extends [size] bytes forward if
   [size] is positive, [size] bytes backwards if [size] is negative,
   or to the end of the file if [size] is zero.
   A write lock prevents any other
   process from acquiring a read or write lock on the region.
   A read lock prevents any other
   process from acquiring a write lock on the region, but lets
   other processes acquire read locks on it.

   The [F_LOCK] and [F_TLOCK] commands attempts to put a write lock
   on the specified region.
   The [F_RLOCK] and [F_TRLOCK] commands attempts to put a read lock
   on the specified region.
   If one or several locks put by another process prevent the current process
   from acquiring the lock, [F_LOCK] and [F_RLOCK] block until these locks
   are removed, while [F_TLOCK] and [F_TRLOCK] fail immediately with an
   exception.
   The [F_ULOCK] removes whatever locks the current process has on
   the specified region.
   Finally, the [F_TEST] command tests whether a write lock can be
   acquired on the specified region, without actually putting a lock.
   It returns immediately if successful, or fails otherwise.

   What happens when a process tries to lock a region of a file that is
   already locked by the same process depends on the OS.  On POSIX-compliant
   systems, the second lock operation succeeds and may "promote" the older
   lock from read lock to write lock.  On Windows, the second lock
   operation will block or fail.
*)


(** {1 Signals}
   Note: installation of signal handlers is performed via
   the functions {!Sys.signal} and {!Sys.set_signal}.
*)

val kill : int -> int -> unit
(** [kill pid sig] sends signal number [sig] to the process
   with id [pid].  On Windows, only the {!Sys.sigkill} signal
   is emulated. *)

type sigprocmask_command =
    SIG_SETMASK
  | SIG_BLOCK
  | SIG_UNBLOCK

val sigprocmask : sigprocmask_command -> int list -> int list
(** [sigprocmask cmd sigs] changes the set of blocked signals.
   If [cmd] is [SIG_SETMASK], blocked signals are set to those in
   the list [sigs].
   If [cmd] is [SIG_BLOCK], the signals in [sigs] are added to
   the set of blocked signals.
   If [cmd] is [SIG_UNBLOCK], the signals in [sigs] are removed
   from the set of blocked signals.
   [sigprocmask] returns the set of previously blocked signals.

   On Windows: not implemented (no inter-process signals on Windows). *)

val sigpending : unit -> int list
(** Return the set of blocked signals that are currently pending.

   On Windows: not implemented (no inter-process signals on Windows). *)

val sigsuspend : int list -> unit
(** [sigsuspend sigs] atomically sets the blocked signals to [sigs]
   and waits for a non-ignored, non-blocked signal to be delivered.
   On return, the blocked signals are reset to their initial value.

   On Windows: not implemented (no inter-process signals on Windows). *)

val pause : unit -> unit
(** Wait until a non-ignored, non-blocked signal is delivered.

  On Windows: not implemented (no inter-process signals on Windows). *)


(** {1 Time functions} *)


type process_times =
  { tms_utime : float;  (** User time for the process *)
    tms_stime : float;  (** System time for the process *)
    tms_cutime : float; (** User time for the children processes *)
    tms_cstime : float; (** System time for the children processes *)
  }
(** The execution times (CPU times) of a process. *)

type tm =
  { tm_sec : int;               (** Seconds 0..60 *)
    tm_min : int;               (** Minutes 0..59 *)
    tm_hour : int;              (** Hours 0..23 *)
    tm_mday : int;              (** Day of month 1..31 *)
    tm_mon : int;               (** Month of year 0..11 *)
    tm_year : int;              (** Year - 1900 *)
    tm_wday : int;              (** Day of week (Sunday is 0) *)
    tm_yday : int;              (** Day of year 0..365 *)
    tm_isdst : bool;            (** Daylight time savings in effect *)
  }
(** The type representing wallclock time and calendar date. *)


val time : unit -> float
(** Return the current time since 00:00:00 GMT, Jan. 1, 1970,
   in seconds. *)

val gettimeofday : unit -> float
(** Same as {!Unix.time}, but with resolution better than 1 second. *)

val gmtime : float -> tm
(** Convert a time in seconds, as returned by {!Unix.time}, into a date and
   a time. Assumes UTC (Coordinated Universal Time), also known as GMT.
   To perform the inverse conversion, set the TZ environment variable
   to "UTC", use {!mktime}, and then restore the original value of TZ. *)

val localtime : float -> tm
(** Convert a time in seconds, as returned by {!Unix.time}, into a date and
   a time. Assumes the local time zone.
   The function performing the inverse conversion is {!mktime}. *)

val mktime : tm -> float * tm
(** Convert a date and time, specified by the [tm] argument, into
   a time in seconds, as returned by {!Unix.time}.  The [tm_isdst],
   [tm_wday] and [tm_yday] fields of [tm] are ignored.  Also return a
   normalized copy of the given [tm] record, with the [tm_wday],
   [tm_yday], and [tm_isdst] fields recomputed from the other fields,
   and the other fields normalized (so that, e.g., 40 October is
   changed into 9 November).  The [tm] argument is interpreted in the
   local time zone. *)

val alarm : int -> int
(** Schedule a [SIGALRM] signal after the given number of seconds.

   On Windows: not implemented. *)

val sleep : int -> unit
(** Stop execution for the given number of seconds. *)

val sleepf : float -> unit
(** Stop execution for the given number of seconds.  Like [sleep],
    but fractions of seconds are supported.

    @since 4.03.0 *)

val times : unit -> process_times
(** Return the execution times of the process.
   On Windows, it is partially implemented, will not report timings
   for child processes. *)

val utimes : string -> float -> float -> unit
(** Set the last access time (second arg) and last modification time
   (third arg) for a file. Times are expressed in seconds from
   00:00:00 GMT, Jan. 1, 1970.  If both times are [0.0], the access
   and last modification times are both set to the current time. *)

type interval_timer =
    ITIMER_REAL
      (** decrements in real time, and sends the signal [SIGALRM] when
         expired.*)
  | ITIMER_VIRTUAL
      (** decrements in process virtual time, and sends [SIGVTALRM]
          when expired. *)
  | ITIMER_PROF
      (** (for profiling) decrements both when the process
         is running and when the system is running on behalf of the
         process; it sends [SIGPROF] when expired. *)
(** The three kinds of interval timers. *)

type interval_timer_status =
  { it_interval : float;         (** Period *)
    it_value : float;            (** Current value of the timer *)
  }
(** The type describing the status of an interval timer *)

val getitimer : interval_timer -> interval_timer_status
(** Return the current status of the given interval timer.

   On Windows: not implemented. *)

val setitimer :
  interval_timer -> interval_timer_status -> interval_timer_status
(** [setitimer t s] sets the interval timer [t] and returns
   its previous status. The [s] argument is interpreted as follows:
   [s.it_value], if nonzero, is the time to the next timer expiration;
   [s.it_interval], if nonzero, specifies a value to
   be used in reloading [it_value] when the timer expires.
   Setting [s.it_value] to zero disables the timer.
   Setting [s.it_interval] to zero causes the timer to be disabled
   after its next expiration.

   On Windows: not implemented. *)


(** {1 User id, group id} *)


val getuid : unit -> int
(** Return the user id of the user executing the process.
   On Windows, always return [1]. *)

val geteuid : unit -> int
(** Return the effective user id under which the process runs.
   On Windows, always return [1]. *)

val setuid : int -> unit
(** Set the real user id and effective user id for the process.
   On Windows: not implemented. *)

val getgid : unit -> int
(** Return the group id of the user executing the process.
   On Windows, always return [1]. *)

val getegid : unit -> int
(** Return the effective group id under which the process runs.
   On Windows, always return [1]. *)

val setgid : int -> unit
(** Set the real group id and effective group id for the process.
   On Windows: not implemented. *)

val getgroups : unit -> int array
(** Return the list of groups to which the user executing the process
   belongs.
   On Windows, always return [[|1|]]. *)

val setgroups : int array -> unit
(** [setgroups groups] sets the supplementary group IDs for the
    calling process. Appropriate privileges are required.
    On Windows: not implemented. *)

val initgroups : string -> int -> unit
(** [initgroups user group] initializes the group access list by
    reading the group database /etc/group and using all groups of
    which [user] is a member. The additional group [group] is also
    added to the list.
    On Windows: not implemented. *)

type passwd_entry =
  { pw_name : string;
    pw_passwd : string;
    pw_uid : int;
    pw_gid : int;
    pw_gecos : string;
    pw_dir : string;
    pw_shell : string
  }
(** Structure of entries in the [passwd] database. *)

type group_entry =
  { gr_name : string;
    gr_passwd : string;
    gr_gid : int;
    gr_mem : string array
  }
(** Structure of entries in the [groups] database. *)

val getlogin : unit -> string
(** Return the login name of the user executing the process. *)

val getpwnam : string -> passwd_entry
(** Find an entry in [passwd] with the given name.
   @raise Not_found if no such entry exist.

   On Windows, always raise [Not_found]. *)

val getgrnam : string -> group_entry
(** Find an entry in [group] with the given name.
   @raise Not_found if no such entry exist.

   On Windows, always raise [Not_found]. *)

val getpwuid : int -> passwd_entry
(** Find an entry in [passwd] with the given user id.
   @raise Not_found if no such entry exist.

   On Windows, always raise [Not_found]. *)

val getgrgid : int -> group_entry
(** Find an entry in [group] with the given group id.
   @raise Not_found if no such entry exist.

   On Windows, always raise [Not_found]. *)


(** {1 Internet addresses} *)


type inet_addr
(** The abstract type of Internet addresses. *)

val inet_addr_of_string : string -> inet_addr
(** Conversion from the printable representation of an Internet
    address to its internal representation.  The argument string
    consists of 4 numbers separated by periods ([XXX.YYY.ZZZ.TTT])
    for IPv4 addresses, and up to 8 numbers separated by colons
    for IPv6 addresses.
    @raise Failure when given a string that does not match these formats. *)

val string_of_inet_addr : inet_addr -> string
(** Return the printable representation of the given Internet address.
    See {!Unix.inet_addr_of_string} for a description of the
    printable representation. *)

val inet_addr_any : inet_addr
(** A special IPv4 address, for use only with [bind], representing
   all the Internet addresses that the host machine possesses. *)

val inet_addr_loopback : inet_addr
(** A special IPv4 address representing the host machine ([127.0.0.1]). *)

val inet6_addr_any : inet_addr
(** A special IPv6 address, for use only with [bind], representing
   all the Internet addresses that the host machine possesses. *)

val inet6_addr_loopback : inet_addr
(** A special IPv6 address representing the host machine ([::1]). *)


(** {1 Sockets} *)


type socket_domain =
    PF_UNIX                     (** Unix domain *)
  | PF_INET                     (** Internet domain (IPv4) *)
  | PF_INET6                    (** Internet domain (IPv6) *)
(** The type of socket domains.  Not all platforms support
    IPv6 sockets (type [PF_INET6]).  Windows does not support
    [PF_UNIX]. *)

type socket_type =
    SOCK_STREAM                 (** Stream socket *)
  | SOCK_DGRAM                  (** Datagram socket *)
  | SOCK_RAW                    (** Raw socket *)
  | SOCK_SEQPACKET              (** Sequenced packets socket *)
(** The type of socket kinds, specifying the semantics of
   communications.  [SOCK_SEQPACKET] is included for completeness,
   but is rarely supported by the OS, and needs system calls that
   are not available in this library. *)

type sockaddr =
    ADDR_UNIX of string
  | ADDR_INET of inet_addr * int
(** The type of socket addresses. [ADDR_UNIX name] is a socket
   address in the Unix domain; [name] is a file name in the file
   system. [ADDR_INET(addr,port)] is a socket address in the Internet
   domain; [addr] is the Internet address of the machine, and
   [port] is the port number. *)

val socket :
    ?cloexec:bool -> socket_domain -> socket_type -> int -> file_descr
(** Create a new socket in the given domain, and with the
   given kind. The third argument is the protocol type; 0 selects
   the default protocol for that kind of sockets.
   See {!set_close_on_exec} for documentation on the [cloexec]
   optional argument. *)

val domain_of_sockaddr: sockaddr -> socket_domain
(** Return the socket domain adequate for the given socket address. *)

val socketpair :
     ?cloexec:bool -> socket_domain -> socket_type -> int ->
                                                 file_descr * file_descr
(** Create a pair of unnamed sockets, connected together.
   See {!set_close_on_exec} for documentation on the [cloexec]
   optional argument. *)

val accept : ?cloexec:bool -> file_descr -> file_descr * sockaddr
(** Accept connections on the given socket. The returned descriptor
   is a socket connected to the client; the returned address is
   the address of the connecting client.
   See {!set_close_on_exec} for documentation on the [cloexec]
   optional argument. *)

val bind : file_descr -> sockaddr -> unit
(** Bind a socket to an address. *)

val connect : file_descr -> sockaddr -> unit
(** Connect a socket to an address. *)

val listen : file_descr -> int -> unit
(** Set up a socket for receiving connection requests. The integer
   argument is the maximal number of pending requests. *)

type shutdown_command =
    SHUTDOWN_RECEIVE            (** Close for receiving *)
  | SHUTDOWN_SEND               (** Close for sending *)
  | SHUTDOWN_ALL                (** Close both *)
(** The type of commands for [shutdown]. *)


val shutdown : file_descr -> shutdown_command -> unit
(** Shutdown a socket connection. [SHUTDOWN_SEND] as second argument
   causes reads on the other end of the connection to return
   an end-of-file condition.
   [SHUTDOWN_RECEIVE] causes writes on the other end of the connection
   to return a closed pipe condition ([SIGPIPE] signal). *)

val getsockname : file_descr -> sockaddr
(** Return the address of the given socket. *)

val getpeername : file_descr -> sockaddr
(** Return the address of the host connected to the given socket. *)

type msg_flag =
    MSG_OOB
  | MSG_DONTROUTE
  | MSG_PEEK
(** The flags for {!Unix.recv},  {!Unix.recvfrom},
   {!Unix.send} and {!Unix.sendto}. *)

val recv : file_descr -> bytes -> int -> int -> msg_flag list -> int
(** Receive data from a connected socket. *)

val recvfrom :
  file_descr -> bytes -> int -> int -> msg_flag list -> int * sockaddr
(** Receive data from an unconnected socket. *)

val send : file_descr -> bytes -> int -> int -> msg_flag list -> int
(** Send data over a connected socket. *)

val send_substring : file_descr -> string -> int -> int -> msg_flag list -> int
(** Same as [send], but take the data from a string instead of a byte
    sequence.
    @since 4.02.0 *)

val sendto :
  file_descr -> bytes -> int -> int -> msg_flag list -> sockaddr -> int
(** Send data over an unconnected socket. *)

val sendto_substring :
  file_descr -> string -> int -> int -> msg_flag list -> sockaddr -> int
(** Same as [sendto], but take the data from a string instead of a
    byte sequence.
    @since 4.02.0 *)


(** {1 Socket options} *)


type socket_bool_option =
    SO_DEBUG       (** Record debugging information *)
  | SO_BROADCAST   (** Permit sending of broadcast messages *)
  | SO_REUSEADDR   (** Allow reuse of local addresses for bind *)
  | SO_KEEPALIVE   (** Keep connection active *)
  | SO_DONTROUTE   (** Bypass the standard routing algorithms *)
  | SO_OOBINLINE   (** Leave out-of-band data in line *)
  | SO_ACCEPTCONN  (** Report whether socket listening is enabled *)
  | TCP_NODELAY    (** Control the Nagle algorithm for TCP sockets *)
  | IPV6_ONLY      (** Forbid binding an IPv6 socket to an IPv4 address *)
(** The socket options that can be consulted with {!Unix.getsockopt}
   and modified with {!Unix.setsockopt}.  These options have a boolean
   ([true]/[false]) value. *)

type socket_int_option =
    SO_SNDBUF      (** Size of send buffer *)
  | SO_RCVBUF      (** Size of received buffer *)
  | SO_ERROR       (** Deprecated.  Use {!Unix.getsockopt_error} instead. *)
  | SO_TYPE        (** Report the socket type *)
  | SO_RCVLOWAT    (** Minimum number of bytes to process for input operations*)
  | SO_SNDLOWAT    (** Minimum number of bytes to process for output
                       operations *)
(** The socket options that can be consulted with {!Unix.getsockopt_int}
   and modified with {!Unix.setsockopt_int}.  These options have an
   integer value. *)

type socket_optint_option =
  SO_LINGER      (** Whether to linger on closed connections
                    that have data present, and for how long
                    (in seconds) *)
(** The socket options that can be consulted with {!Unix.getsockopt_optint}
   and modified with {!Unix.setsockopt_optint}.  These options have a
   value of type [int option], with [None] meaning ``disabled''. *)

type socket_float_option =
    SO_RCVTIMEO    (** Timeout for input operations *)
  | SO_SNDTIMEO    (** Timeout for output operations *)
(** The socket options that can be consulted with {!Unix.getsockopt_float}
   and modified with {!Unix.setsockopt_float}.  These options have a
   floating-point value representing a time in seconds.
   The value 0 means infinite timeout. *)

val getsockopt : file_descr -> socket_bool_option -> bool
(** Return the current status of a boolean-valued option
   in the given socket. *)

val setsockopt : file_descr -> socket_bool_option -> bool -> unit
(** Set or clear a boolean-valued option in the given socket. *)

val getsockopt_int : file_descr -> socket_int_option -> int
(** Same as {!Unix.getsockopt} for an integer-valued socket option. *)

val setsockopt_int : file_descr -> socket_int_option -> int -> unit
(** Same as {!Unix.setsockopt} for an integer-valued socket option. *)

val getsockopt_optint : file_descr -> socket_optint_option -> int option
(** Same as {!Unix.getsockopt} for a socket option whose value is an
   [int option]. *)

val setsockopt_optint :
      file_descr -> socket_optint_option -> int option -> unit
(** Same as {!Unix.setsockopt} for a socket option whose value is an
   [int option]. *)

val getsockopt_float : file_descr -> socket_float_option -> float
(** Same as {!Unix.getsockopt} for a socket option whose value is a
   floating-point number. *)

val setsockopt_float : file_descr -> socket_float_option -> float -> unit
(** Same as {!Unix.setsockopt} for a socket option whose value is a
   floating-point number. *)

val getsockopt_error : file_descr -> error option
(** Return the error condition associated with the given socket,
    and clear it. *)


(** {1 High-level network connection functions} *)


val open_connection : sockaddr -> in_channel * out_channel
(** Connect to a server at the given address.
   Return a pair of buffered channels connected to the server.
   Remember to call {!Pervasives.flush} on the output channel at the right
   times to ensure correct synchronization. *)

val shutdown_connection : in_channel -> unit
(** ``Shut down'' a connection established with {!Unix.open_connection};
   that is, transmit an end-of-file condition to the server reading
   on the other side of the connection. This does not fully close the
   file descriptor associated with the channel, which you must remember
   to free via {!Pervasives.close_in}. *)

val establish_server : (in_channel -> out_channel -> unit) -> sockaddr -> unit
(** Establish a server on the given address.
   The function given as first argument is called for each connection
   with two buffered channels connected to the client. A new process
   is created for each connection. The function {!Unix.establish_server}
   never returns normally.

   On Windows, it is not implemented.  Use threads. *)


(** {1 Host and protocol databases} *)


type host_entry =
  { h_name : string;
    h_aliases : string array;
    h_addrtype : socket_domain;
    h_addr_list : inet_addr array
  }
(** Structure of entries in the [hosts] database. *)

type protocol_entry =
  { p_name : string;
    p_aliases : string array;
    p_proto : int
  }
(** Structure of entries in the [protocols] database. *)

type service_entry =
  { s_name : string;
    s_aliases : string array;
    s_port : int;
    s_proto : string
  }
(** Structure of entries in the [services] database. *)

val gethostname : unit -> string
(** Return the name of the local host. *)

val gethostbyname : string -> host_entry
(** Find an entry in [hosts] with the given name.
    @raise Not_found if no such entry exist. *)

val gethostbyaddr : inet_addr -> host_entry
(** Find an entry in [hosts] with the given address.
    @raise Not_found if no such entry exist. *)

val getprotobyname : string -> protocol_entry
(** Find an entry in [protocols] with the given name.
    @raise Not_found if no such entry exist. *)

val getprotobynumber : int -> protocol_entry
(** Find an entry in [protocols] with the given protocol number.
    @raise Not_found if no such entry exist. *)

val getservbyname : string -> string -> service_entry
(** Find an entry in [services] with the given name.
    @raise Not_found if no such entry exist. *)

val getservbyport : int -> string -> service_entry
(** Find an entry in [services] with the given service number.
    @raise Not_found if no such entry exist. *)

type addr_info =
  { ai_family : socket_domain;          (** Socket domain *)
    ai_socktype : socket_type;          (** Socket type *)
    ai_protocol : int;                  (** Socket protocol number *)
    ai_addr : sockaddr;                 (** Address *)
    ai_canonname : string               (** Canonical host name  *)
  }
(** Address information returned by {!Unix.getaddrinfo}. *)

type getaddrinfo_option =
    AI_FAMILY of socket_domain          (** Impose the given socket domain *)
  | AI_SOCKTYPE of socket_type          (** Impose the given socket type *)
  | AI_PROTOCOL of int                  (** Impose the given protocol  *)
  | AI_NUMERICHOST                      (** Do not call name resolver,
                                            expect numeric IP address *)
  | AI_CANONNAME                        (** Fill the [ai_canonname] field
                                            of the result *)
  | AI_PASSIVE                          (** Set address to ``any'' address
                                            for use with {!Unix.bind} *)
(** Options to {!Unix.getaddrinfo}. *)

val getaddrinfo:
  string -> string -> getaddrinfo_option list -> addr_info list
(** [getaddrinfo host service opts] returns a list of {!Unix.addr_info}
    records describing socket parameters and addresses suitable for
    communicating with the given host and service.  The empty list is
    returned if the host or service names are unknown, or the constraints
    expressed in [opts] cannot be satisfied.

    [host] is either a host name or the string representation of an IP
    address.  [host] can be given as the empty string; in this case,
    the ``any'' address or the ``loopback'' address are used,
    depending whether [opts] contains [AI_PASSIVE].
    [service] is either a service name or the string representation of
    a port number.  [service] can be given as the empty string;
    in this case, the port field of the returned addresses is set to 0.
    [opts] is a possibly empty list of options that allows the caller
    to force a particular socket domain (e.g. IPv6 only or IPv4 only)
    or a particular socket type (e.g. TCP only or UDP only). *)

type name_info =
  { ni_hostname : string;               (** Name or IP address of host *)
    ni_service : string;                (** Name of service or port number *)
  }
(** Host and service information returned by {!Unix.getnameinfo}. *)

type getnameinfo_option =
    NI_NOFQDN            (** Do not qualify local host names *)
  | NI_NUMERICHOST       (** Always return host as IP address *)
  | NI_NAMEREQD          (** Fail if host name cannot be determined *)
  | NI_NUMERICSERV       (** Always return service as port number *)
  | NI_DGRAM             (** Consider the service as UDP-based
                             instead of the default TCP *)
(** Options to {!Unix.getnameinfo}. *)

val getnameinfo : sockaddr -> getnameinfo_option list -> name_info
(** [getnameinfo addr opts] returns the host name and service name
    corresponding to the socket address [addr].  [opts] is a possibly
    empty list of options that governs how these names are obtained.
    @raise Not_found if an error occurs. *)


(** {1 Terminal interface} *)


(** The following functions implement the POSIX standard terminal
   interface. They provide control over asynchronous communication ports
   and pseudo-terminals. Refer to the [termios] man page for a
   complete description. *)

type terminal_io =
  {
    (* input modes *)
    mutable c_ignbrk : bool;  (** Ignore the break condition. *)
    mutable c_brkint : bool;  (** Signal interrupt on break condition. *)
    mutable c_ignpar : bool;  (** Ignore characters with parity errors. *)
    mutable c_parmrk : bool;  (** Mark parity errors. *)
    mutable c_inpck : bool;   (** Enable parity check on input. *)
    mutable c_istrip : bool;  (** Strip 8th bit on input characters. *)
    mutable c_inlcr : bool;   (** Map NL to CR on input. *)
    mutable c_igncr : bool;   (** Ignore CR on input. *)
    mutable c_icrnl : bool;   (** Map CR to NL on input. *)
    mutable c_ixon : bool;    (** Recognize XON/XOFF characters on input. *)
    mutable c_ixoff : bool;   (** Emit XON/XOFF chars to control input flow. *)
    (* Output modes: *)
    mutable c_opost : bool;   (** Enable output processing. *)
    (* Control modes: *)
    mutable c_obaud : int;    (** Output baud rate (0 means close connection).*)
    mutable c_ibaud : int;    (** Input baud rate. *)
    mutable c_csize : int;    (** Number of bits per character (5-8). *)
    mutable c_cstopb : int;   (** Number of stop bits (1-2). *)
    mutable c_cread : bool;   (** Reception is enabled. *)
    mutable c_parenb : bool;  (** Enable parity generation and detection. *)
    mutable c_parodd : bool;  (** Specify odd parity instead of even. *)
    mutable c_hupcl : bool;   (** Hang up on last close. *)
    mutable c_clocal : bool;  (** Ignore modem status lines. *)
    (* Local modes: *)
    mutable c_isig : bool;    (** Generate signal on INTR, QUIT, SUSP. *)
    mutable c_icanon : bool;  (** Enable canonical processing
                                 (line buffering and editing) *)
    mutable c_noflsh : bool;  (** Disable flush after INTR, QUIT, SUSP. *)
    mutable c_echo : bool;    (** Echo input characters. *)
    mutable c_echoe : bool;   (** Echo ERASE (to erase previous character). *)
    mutable c_echok : bool;   (** Echo KILL (to erase the current line). *)
    mutable c_echonl : bool;  (** Echo NL even if c_echo is not set. *)
    (* Control characters: *)
    mutable c_vintr : char;   (** Interrupt character (usually ctrl-C). *)
    mutable c_vquit : char;   (** Quit character (usually ctrl-\). *)
    mutable c_verase : char;  (** Erase character (usually DEL or ctrl-H). *)
    mutable c_vkill : char;   (** Kill line character (usually ctrl-U). *)
    mutable c_veof : char;    (** End-of-file character (usually ctrl-D). *)
    mutable c_veol : char;    (** Alternate end-of-line char. (usually none). *)
    mutable c_vmin : int;     (** Minimum number of characters to read
                                 before the read request is satisfied. *)
    mutable c_vtime : int;    (** Maximum read wait (in 0.1s units). *)
    mutable c_vstart : char;  (** Start character (usually ctrl-Q). *)
    mutable c_vstop : char;   (** Stop character (usually ctrl-S). *)
  }

val tcgetattr : file_descr -> terminal_io
(** Return the status of the terminal referred to by the given
   file descriptor.
   On Windows, not implemented. *)

type setattr_when =
    TCSANOW
  | TCSADRAIN
  | TCSAFLUSH

val tcsetattr : file_descr -> setattr_when -> terminal_io -> unit
(** Set the status of the terminal referred to by the given
   file descriptor. The second argument indicates when the
   status change takes place: immediately ([TCSANOW]),
   when all pending output has been transmitted ([TCSADRAIN]),
   or after flushing all input that has been received but not
   read ([TCSAFLUSH]). [TCSADRAIN] is recommended when changing
   the output parameters; [TCSAFLUSH], when changing the input
   parameters.

   On Windows, not implemented. *)

val tcsendbreak : file_descr -> int -> unit
(** Send a break condition on the given file descriptor.
   The second argument is the duration of the break, in 0.1s units;
   0 means standard duration (0.25s).

   On Windows, not implemented. *)

val tcdrain : file_descr -> unit
(** Waits until all output written on the given file descriptor
   has been transmitted.

   On Windows, not implemented. *)

type flush_queue =
    TCIFLUSH
  | TCOFLUSH
  | TCIOFLUSH

val tcflush : file_descr -> flush_queue -> unit
(** Discard data written on the given file descriptor but not yet
   transmitted, or data received but not yet read, depending on the
   second argument: [TCIFLUSH] flushes data received but not read,
   [TCOFLUSH] flushes data written but not transmitted, and
   [TCIOFLUSH] flushes both.

   On Windows, not implemented. *)

type flow_action =
    TCOOFF
  | TCOON
  | TCIOFF
  | TCION

val tcflow : file_descr -> flow_action -> unit
(** Suspend or restart reception or transmission of data on
   the given file descriptor, depending on the second argument:
   [TCOOFF] suspends output, [TCOON] restarts output,
   [TCIOFF] transmits a STOP character to suspend input,
   and [TCION] transmits a START character to restart input.

   On Windows, not implemented. *)

val setsid : unit -> int
(** Put the calling process in a new session and detach it from
   its controlling terminal.

   On Windows, not implemented. *)
