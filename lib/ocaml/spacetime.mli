(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2015--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Profiling of a program's space behaviour over time.
    Currently only supported on x86-64 platforms running 64-bit code.

    To use the functions in this module you must:
    - configure the compiler with "-spacetime";
    - compile to native code.
    Without these conditions being satisfied the functions in this module
    will have no effect.

    Instead of manually taking profiling heap snapshots with this module it is
    possible to use an automatic snapshot facility that writes profiling
    information at fixed intervals to a file. To enable this, all that needs to
    be done is to build the relevant program using a compiler configured with
    -spacetime; and set the environment variable OCAML_SPACETIME_INTERVAL to an
    integer number of milliseconds giving the interval between profiling heap
    snapshots. This interval should not be made excessively small relative to
    the running time of the program. A typical interval to start with might be
    1/100 of the running time of the program.  The program must exit "normally"
    (i.e. by calling [exit], with whatever exit code, rather than being
    abnormally terminated by a signal) so that the snapshot file is
    correctly completed.

    When using the automatic snapshot mode the profiling output is written
    to a file called "spacetime-<pid>" where <pid> is the process ID of the
    program.  (If the program forks and continues executing then multiple
    files may be produced with different pid numbers.)  The profiling output
    is by default written to the current working directory when the program
    starts.  This may be customised by setting the OCAML_SPACETIME_SNAPSHOT_DIR
    environment variable to the name of the desired directory.

    If using automatic snapshots the presence of the
    [save_event_for_automatic_snapshots] function, below, should be noted.

    The functions in this module are thread safe.

    For functions to decode the information recorded by the profiler,
    see the Spacetime offline library in otherlibs/. *)

(** [enabled] is [true] if the compiler is configured with spacetime and [false]
    otherwise *)
val enabled : bool

module Series : sig
  (** Type representing a file that will hold a series of heap snapshots
      together with additional information required to interpret those
      snapshots. *)
  type t

  (** [create ~path] creates a series file at [path]. *)
  val create : path:string -> t

  (** [save_event] writes an event, which is an arbitrary string, into the
      given series file.  This may be used for identifying particular points
      during program execution when analysing the profile.
      The optional [time] parameter is as for {!Snapshot.take}.
  *)
  val save_event : ?time:float -> t -> event_name:string -> unit

  (** [save_and_close series] writes information into [series] required for
      interpreting the snapshots that [series] contains and then closes the
      [series] file. This function must be called to produce a valid series
      file.
      The optional [time] parameter is as for {!Snapshot.take}.
  *)
  val save_and_close : ?time:float -> t -> unit
end

module Snapshot : sig
  (** [take series] takes a snapshot of the profiling annotations on the values
      in the minor and major heaps, together with GC stats, and write the
      result to the [series] file.  This function triggers a minor GC but does
      not allocate any memory itself.
      If the optional [time] is specified, it will be used instead of the
      result of {!Sys.time} as the timestamp of the snapshot.  Such [time]s
      should start from zero and be monotonically increasing.  This parameter
      is intended to be used so that snapshots can be correlated against wall
      clock time (which is not supported in the standard library) rather than
      elapsed CPU time.
  *)
  val take : ?time:float -> Series.t -> unit
end

(** Like {!Series.save_event}, but writes to the automatic snapshot file.
    This function is a no-op if OCAML_SPACETIME_INTERVAL was not set. *)
val save_event_for_automatic_snapshots : event_name:string -> unit
