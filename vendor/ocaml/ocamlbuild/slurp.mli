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


(* Original author: Berke Durak *)
(* Slurp *)

(** Scans a directory lazily to build a tree that can be user-decorated. *)

type 'a entry =
    Dir of string * string * My_unix.stats Lazy.t * 'a * 'a entry list Lazy.t
    (** [Dir(path, name, lst, decoration, lentries)] is a directory named [name] whose path is [path].
       Its stat is lazily stored in [lst] and its entries are lazily scanned in [lentries].  [decoration]
       is of type 'a. *)
  | File of string * string * My_unix.stats Lazy.t * 'a
    (** [File(path, name, lst, decoration)] is a file named [name] whose path is [path].
       Its stat is lazily stored in [lst].  [decoration] is of type 'a. *)
  | Error of exn
    (** [Error x] means that the exception [x] was raised while scanning or statting an entry. *)
  | Nothing
    (** Convenient when filtering out entries. *)

(** Recursively scan the filesystem starting at the given directory. *)
val slurp : string -> unit entry

(** [filter f entry] only retains from [entry] the nodes for which
    [f path name] returns [true]. *)
val filter : (string -> string -> 'a -> bool) -> 'a entry -> 'a entry

(** [map f entries] changes the decoration in [entries] by applying
    [f] to them. [f] is called as [f path name decoration]. *)
val map : (string -> string -> 'a -> 'b) -> 'a entry -> 'b entry

(** [fold f entry x] iterates [f] over the entries and an accumulator
    initially containing [x]; at each iteration, [f] gets the current
    value of the accumulator and returns its new value. *)
val fold : (string -> string -> 'b -> 'a -> 'a) -> 'b entry -> 'a -> 'a

(** Force the evaluation of the whole entry. *)
val force : 'a entry -> unit
