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

(** System interface.

  Every function in this module raises [Sys_error] with an
  informative message when the underlying system call signal
  an error.
*)

val argv : string array
(** The command line arguments given to the process.
   The first element is the command name used to invoke the program.
   The following elements are the command-line arguments
   given to the program. *)

val executable_name : string
(** The name of the file containing the executable currently running. *)

external file_exists : string -> bool = "caml_sys_file_exists"
(** Test if a file with the given name exists. *)

external is_directory : string -> bool = "caml_sys_is_directory"
(** Returns [true] if the given name refers to a directory,
    [false] if it refers to another kind of file.
    Raise [Sys_error] if no file exists with the given name.
    @since 3.10.0
*)

external remove : string -> unit = "caml_sys_remove"
(** Remove the given file name from the file system. *)

external rename : string -> string -> unit = "caml_sys_rename"
(** Rename a file.  [rename oldpath newpath] renames the file
    called [oldpath], giving it [newpath] as its new name,
    moving it between directories if needed.  If [newpath] already
    exists, its contents will be replaced with those of [oldpath].
    Depending on the operating system, the metadata (permissions,
    owner, etc) of [newpath] can either be preserved or be replaced by
    those of [oldpath].
   @since 4.06 concerning the "replace existing file" behavior *)

external getenv : string -> string = "caml_sys_getenv"
(** Return the value associated to a variable in the process
   environment. Raise [Not_found] if the variable is unbound. *)

val getenv_opt: string -> string option
(** Return the value associated to a variable in the process
    environment or [None] if the variable is unbound.
    @since 4.05
*)

external command : string -> int = "caml_sys_system_command"
(** Execute the given shell command and return its exit code. *)

external time : unit -> (float [@unboxed]) =
  "caml_sys_time" "caml_sys_time_unboxed" [@@noalloc]
(** Return the processor time, in seconds, used by the program
   since the beginning of execution. *)

external chdir : string -> unit = "caml_sys_chdir"
(** Change the current working directory of the process. *)

external getcwd : unit -> string = "caml_sys_getcwd"
(** Return the current working directory of the process. *)

external readdir : string -> string array = "caml_sys_read_directory"
(** Return the names of all files present in the given directory.
   Names denoting the current directory and the parent directory
   (["."] and [".."] in Unix) are not returned.  Each string in the
   result is a file name rather than a complete path.  There is no
   guarantee that the name strings in the resulting array will appear
   in any specific order; they are not, in particular, guaranteed to
   appear in alphabetical order. *)

val interactive : bool ref
(** This reference is initially set to [false] in standalone
   programs and to [true] if the code is being executed under
   the interactive toplevel system [ocaml]. *)

val os_type : string
(** Operating system currently executing the OCaml program. One of
-  ["Unix"] (for all Unix versions, including Linux and Mac OS X),
-  ["Win32"] (for MS-Windows, OCaml compiled with MSVC++ or Mingw),
-  ["Cygwin"] (for MS-Windows, OCaml compiled with Cygwin). *)

type backend_type =
  | Native
  | Bytecode
  | Other of string (**)
(** Currently, the official distribution only supports [Native] and
    [Bytecode], but it can be other backends with alternative
    compilers, for example, javascript.

    @since 4.04.0
*)

val backend_type : backend_type
(** Backend type  currently executing the OCaml program.
    @since 4.04.0
 *)

val unix : bool
(** True if [Sys.os_type = "Unix"].
    @since 4.01.0 *)

val win32 : bool
(** True if [Sys.os_type = "Win32"].
    @since 4.01.0 *)

val cygwin : bool
(** True if [Sys.os_type = "Cygwin"].
    @since 4.01.0 *)

val word_size : int
(** Size of one word on the machine currently executing the OCaml
   program, in bits: 32 or 64. *)

val int_size : int
(** Size of an int.  It is 31 bits (resp. 63 bits) when using the
    OCaml compiler on a 32 bits (resp. 64 bits) platform.  It may
    differ for other compilers, e.g. it is 32 bits when compiling to
    JavaScript.
    @since 4.03.0 *)

val big_endian : bool
(** Whether the machine currently executing the Caml program is big-endian.
    @since 4.00.0 *)

val max_string_length : int
(** Maximum length of strings and byte sequences. *)

val max_array_length : int
(** Maximum length of a normal array.  The maximum length of a float
    array is [max_array_length/2] on 32-bit machines and
    [max_array_length] on 64-bit machines. *)

external runtime_variant : unit -> string = "caml_runtime_variant"
(** Return the name of the runtime variant the program is running on.
    This is normally the argument given to [-runtime-variant] at compile
    time, but for byte-code it can be changed after compilation.
    @since 4.03.0 *)

external runtime_parameters : unit -> string = "caml_runtime_parameters"
(** Return the value of the runtime parameters, in the same format
    as the contents of the [OCAMLRUNPARAM] environment variable.
    @since 4.03.0 *)


(** {1 Signal handling} *)


type signal_behavior =
    Signal_default
  | Signal_ignore
  | Signal_handle of (int -> unit)   (** *)
(** What to do when receiving a signal:
   - [Signal_default]: take the default behavior
     (usually: abort the program)
   - [Signal_ignore]: ignore the signal
   - [Signal_handle f]: call function [f], giving it the signal
   number as argument. *)

external signal :
  int -> signal_behavior -> signal_behavior = "caml_install_signal_handler"
(** Set the behavior of the system on receipt of a given signal.  The
   first argument is the signal number.  Return the behavior
   previously associated with the signal. If the signal number is
   invalid (or not available on your system), an [Invalid_argument]
   exception is raised. *)

val set_signal : int -> signal_behavior -> unit
(** Same as {!Sys.signal} but return value is ignored. *)


(** {2 Signal numbers for the standard POSIX signals.} *)

val sigabrt : int
(** Abnormal termination *)

val sigalrm : int
(** Timeout *)

val sigfpe : int
(** Arithmetic exception *)

val sighup : int
(** Hangup on controlling terminal *)

val sigill : int
(** Invalid hardware instruction *)

val sigint : int
(** Interactive interrupt (ctrl-C) *)

val sigkill : int
(** Termination (cannot be ignored) *)

val sigpipe : int
(** Broken pipe *)

val sigquit : int
(** Interactive termination *)

val sigsegv : int
(** Invalid memory reference *)

val sigterm : int
(** Termination *)

val sigusr1 : int
(** Application-defined signal 1 *)

val sigusr2 : int
(** Application-defined signal 2 *)

val sigchld : int
(** Child process terminated *)

val sigcont : int
(** Continue *)

val sigstop : int
(** Stop *)

val sigtstp : int
(** Interactive stop *)

val sigttin : int
(** Terminal read from background process *)

val sigttou : int
(** Terminal write from background process *)

val sigvtalrm : int
(** Timeout in virtual time *)

val sigprof : int
(** Profiling interrupt *)

val sigbus : int
(** Bus error
    @since 4.03 *)

val sigpoll : int
(** Pollable event
    @since 4.03 *)

val sigsys : int
(** Bad argument to routine
    @since 4.03 *)

val sigtrap : int
(** Trace/breakpoint trap
    @since 4.03 *)

val sigurg : int
(** Urgent condition on socket
    @since 4.03 *)

val sigxcpu : int
(** Timeout in cpu time
    @since 4.03 *)

val sigxfsz : int
(** File size limit exceeded
    @since 4.03 *)


exception Break
(** Exception raised on interactive interrupt if {!Sys.catch_break}
   is on. *)


val catch_break : bool -> unit
(** [catch_break] governs whether interactive interrupt (ctrl-C)
   terminates the program or raises the [Break] exception.
   Call [catch_break true] to enable raising [Break],
   and [catch_break false] to let the system
   terminate the program on user interrupt. *)


val ocaml_version : string
(** [ocaml_version] is the version of OCaml.
    It is a string of the form ["major.minor[.patchlevel][+additional-info]"],
    where [major], [minor], and [patchlevel] are integers, and
    [additional-info] is an arbitrary string. The [[.patchlevel]] and
    [[+additional-info]] parts may be absent. *)


val enable_runtime_warnings: bool -> unit
(** Control whether the OCaml runtime system can emit warnings
    on stderr.  Currently, the only supported warning is triggered
    when a channel created by [open_*] functions is finalized without
    being closed.  Runtime warnings are enabled by default.

    @since 4.03.0 *)

val runtime_warnings_enabled: unit -> bool
(** Return whether runtime warnings are currently enabled.

    @since 4.03.0 *)

(** {1 Optimization} *)

external opaque_identity : 'a -> 'a = "%opaque"
(** For the purposes of optimization, [opaque_identity] behaves like an
    unknown (and thus possibly side-effecting) function.

    At runtime, [opaque_identity] disappears altogether.

    A typical use of this function is to prevent pure computations from being
    optimized away in benchmarking loops.  For example:
    {[
      for _round = 1 to 100_000 do
        ignore (Sys.opaque_identity (my_pure_computation ()))
      done
    ]}

    @since 4.03.0
*)
