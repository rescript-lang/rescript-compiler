(**************************************************************************)
(*                                                                        *)
(*  This file is part of OcamlGraph.                                      *)
(*                                                                        *)
(*  Copyright (C) 2009-2010                                               *)
(*    CEA (Commissariat à l'Énergie Atomique)                             *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1, with a linking exception.                    *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the file ../LICENSE for more details.                             *)
(*                                                                        *)
(*  Authors:                                                              *)
(*    - Julien Signoles  (Julien.Signoles@cea.fr)                         *)
(*    - Jean-Denis Koeck (jdkoeck@gmail.com)                              *)
(*    - Benoit Bataille  (benoit.bataille@gmail.com)                      *)
(*                                                                        *)
(**************************************************************************)

(** View classes.

    Each optional function [delay_node], [delay_edge] and [delay_cluster] of
    this module may be used to indicate whether an element must be displayed
    instantaneously (if the function returns [false]) or may be delayed for
    latter display (if the function returns [true]). By default, each function
    always returns [false]. It may be set for returning [true] from time to
    time, improving efficiency. *)

open DGraphViewItem

(** Graph widget derived from [GnoCanvas.canvas].
    Support zooming and scrolling. *)
class type ['vertex, 'edge, 'cluster] view = object

  inherit GnoCanvas.canvas

  method model : ('vertex, 'edge, 'cluster) DGraphModel.abstract_model

  (** {2 Getters} *)

  method get_node : 'vertex -> 'vertex view_item
  method get_edge : 'edge -> 'edge view_item
  method get_cluster : 'cluster -> 'cluster view_item

  (** {2 Iterators} *)

  method iter_nodes:  ('vertex view_item -> unit) -> unit
  method iter_edges: ('vertex view_item -> 'vertex view_item -> unit) -> unit
  method iter_edges_e:  ('edge view_item -> unit) -> unit
  method iter_clusters: ('cluster view_item -> unit) -> unit

  method iter_succ: ('vertex view_item -> unit) -> 'vertex view_item -> unit
  method iter_pred: ('vertex view_item -> unit) -> 'vertex view_item -> unit
  method iter_succ_e: ('edge view_item -> unit) -> 'vertex view_item -> unit
  method iter_pred_e: ('edge view_item -> unit) -> 'vertex view_item -> unit

  (* Benoit Bataille's method: is it really useful? *)
  method iter_associated_vertex:
    ('vertex view_item -> unit) -> 'vertex view_item -> unit

  (** {2 Membership functions} *)

  method mem_edge: 'vertex view_item -> 'vertex view_item -> bool
  method find_edge: 'vertex view_item -> 'vertex view_item -> 'edge view_item
  method src: 'edge view_item -> 'vertex view_item
  method dst: 'edge view_item -> 'vertex view_item

  (** {2 Zooming} *)

  method zoom_factor : float
  (** The current zoom factor.*)

  method zoom_to : float -> unit
  (** Set an absolute zoom factor.*)

  method zoom_in : unit -> unit
  (** Increase [zoom_factor] by [zoom_factor*zoom_padding].*)

  method zoom_out : unit -> unit
  (** Decrease [zoom_factor] by [zoom_factor*zoom_padding].*)

  method adapt_zoom : unit -> unit
  (** Zoom in order to view the whole graph (bird eye view). *)

  method set_zoom_padding: float -> unit
  (** Set the zoom padding used by [zoom_in] and [zoom_out]. 
      It defaults to 0.1. *)

  method center_node: 'vertex view_item -> unit
  (** Center canvas on a node. *)

  (** {2 Highlighting} *)

  method connect_highlighting_event: unit -> unit

  method highlight: ?color: int32 * int32 -> 'vertex view_item -> unit
  (** Change the color of the given vertex item.
      May be cancelled by [dehighlight].
      If [color] is [primary,secondary], then
      [primary] is used except if the current color is [primary]. In this
      case, [secondary] is used. *)

  method dehighlight: 'vertex view_item -> unit
  (** Cancel [highlight]. *)

end

module type S = sig

  type vertex
  type edge
  type cluster

  val view:
    ?aa:bool (** Anti-aliasing *) ->
    ?delay_node:(vertex -> bool) ->
    ?delay_edge:(edge -> bool) ->
    ?delay_cluster:(cluster -> bool) ->
    ?border_width:int ->
    ?width:int ->
    ?height:int ->
    ?packing:(GObj.widget -> unit) ->
    ?show:bool ->
    (vertex, edge, cluster) DGraphModel.abstract_model ->
    (vertex, edge, cluster) view
    (** View as a Gnome Canvas.
        Support zooming and scrolling. *)

end

module Make(V: Sig.HASHABLE)(E: Sig.HASHABLE)(C: Sig.HASHABLE) :
  S with type vertex = V.t and type edge = E.t and type cluster = C.t
