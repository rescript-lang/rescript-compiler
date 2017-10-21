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

(** This file provide useful function to build windows to put the graph *)

val create_scrolled_canvas : (GObj.widget -> unit) -> GnoCanvas.canvas

val create_graph_win : string -> ViewGraph_select.t_env

