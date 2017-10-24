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

(** ViewGraph : a library to view .dot graphs and interact with the GUI. 
 * To use it you have :
 * - first to define callbacks (see {!modtype:ViewGraph.SigCb})
 * - then instanciate the module {!module:ViewGraph.M} with your callbacks,
 * - then use {!ViewGraph.M.open_dot_file}
 * - don't forget to call {!ViewGraph.M.clear} when changing the file.
*)

(** raised when the call to a dot command fails.
 * The string gives the command that failed *)
exception DotError of string

type t_point = float * float
type t_coord = t_point * t_point

type t_graph
type t_node 

type t_gtk_obj = GnomeCanvas.re_p GnoCanvas.item

(*val get_canvas : t_graph -> GnoCanvas.canvas *)

val get_id : t_node -> string
val get_coord : t_node -> t_coord option
val get_obj : t_node -> t_gtk_obj option

(** @return 2 lists : the predecessors and successors of the node*)
val get_neighbours : t_graph -> t_node -> t_node list * t_node list

module type SigCb = sig
  type t_env

  val button_one_press_on_graph : t_env -> unit
  val button_two_press_on_graph : t_env -> unit
  val button_three_press_on_graph : t_env -> unit
  val button_one_press_on_node : t_env -> t_node -> unit
  val button_two_press_on_node : t_env -> t_node -> unit
  val button_three_press_on_node : t_env -> t_node -> unit
  val enter_node : t_env -> t_node -> unit
  val leave_node : t_env -> t_node -> unit
end

(** usefull when we don't want to have callbacks on the nodes *)
module EmptyCb : SigCb with type t_env=unit

module M (Cb : SigCb) : sig

  (** Open the dot file in the canvas.
   * @raise Error if either the image or the graph fail to build *)
  val open_dot_file : Cb.t_env -> GnoCanvas.canvas -> 
    ?dot_cmd:string -> string -> t_graph

  (** it is very important to not using the graph anymore after calling [clear] *)
  val clear : GnoCanvas.canvas -> t_graph -> unit
end
