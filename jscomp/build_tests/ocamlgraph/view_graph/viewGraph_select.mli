(**************************************************************************)
(*                                                                        *)
(*  ViewGraph: a library to interact with graphs in ocaml and lablgtk2    *)
(*                                                                        *)
(*  Copyright (C) 2008 - Anne Pacalet                                     *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2, with the special exception on linking              *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(** This module can be used to add some selection feature to {!ViewGraph}. 
 * See {!ViewGraph_test} to see how to use it.
 * *)

(** object returned by [init] and used by [open_dot_file] *)
type t_env

type t_options = {
  sel_1_color : string; (** color of the selected node (default "red") *)
  sel_2_color : string; (** color of the selected neighbour (default "green") *)
  center_node_when_selected :bool; 
  (** automatically center a node when it is selected (default true) *)
}

(** some default values for the options.
 * It is a good idea to use it and overwrite the one that have to be changed,
 * just in case some more options appear *)
val default_options : t_options

(** should be called only once because it creates widgets.
 * The packing function is the place to put the messages about selection.
*)
val init : t_options -> GnoCanvas.canvas -> (GObj.widget -> unit) -> t_env

(** functor to instanciate with your callbacks *)
module VG (UserCb : ViewGraph_core.SigCb) : sig
  val open_dot_file : UserCb.t_env -> t_env -> 
    ?dot_cmd:string -> string -> ViewGraph_core.t_graph
end

(** Popup a message window with some help. *)
val show_help : unit -> unit
