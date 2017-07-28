(***********************************************************************)
(*                                                                     *)
(*                             ocamlbuild                              *)
(*                                                                     *)
(*  Nicolas Pouillard, Berke Durak, projet Gallium, INRIA Rocquencourt *)
(*                                                                     *)
(*  Copyright 2007 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)


(* Original author: Nicolas Pouillard *)
type file_kind =
| FK_dir
| FK_file
| FK_link
| FK_other

type stats =
  {
    stat_file_kind : file_kind;
    stat_key       : string
  }

val is_degraded : bool Lazy.t

val is_link : string -> bool
val run_and_open : string -> (in_channel -> 'a) -> 'a
val readlink : string -> string
val run_and_read : string -> string

(** See [Ocamlbuild_executor.execute] *)
val execute_many :
  ?max_jobs:int ->
  ?ticker:(unit -> unit) ->
  ?period:float ->
  ?display:((out_channel -> unit) -> unit) ->
    ((unit -> string) list list) ->
    (bool list * exn) option

val report_error : Format.formatter -> exn -> unit
val at_exit_once : (unit -> unit) -> unit

val gettimeofday : unit -> float

val stdout_isatty : unit -> bool

val stat : string -> stats
val lstat : string -> stats

(** internal usage only *)
type implem =
  {
    mutable is_degraded   : bool;
    mutable is_link       : string -> bool;
    mutable run_and_open  : 'a . string -> (in_channel -> 'a) -> 'a;
    mutable readlink      : string -> string;
    mutable execute_many  : ?max_jobs:int ->
                            ?ticker:(unit -> unit) ->
                            ?period:float ->
                            ?display:((out_channel -> unit) -> unit) ->
                            ((unit -> string) list list) ->
                            (bool list * exn) option;
    mutable report_error  : Format.formatter -> exn -> unit;
    mutable at_exit_once  : (unit -> unit) -> unit;
    mutable gettimeofday  : unit -> float;
    mutable stdout_isatty : unit -> bool;
    mutable stat          : string -> stats;
    mutable lstat         : string -> stats;
  }

val implem : implem
