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

(** Reads layout information from xdot ASTs *)

open Graph

(** Simple layout types *)

(** 2D coordinates *)
type pos = float * float

(** upper-left and bottom-right corners *)
type bounding_box = pos * pos

(**
   Layout informations are parsed from xdot files
   (dot files with graphviz layout).

   Each node or edge layout thus contains several lists of
   drawing operations.

   See http://www.graphviz.org/doc/info/output.html#d:xdot
   to understand the details of the layout informations.

*)

(** Each node has at least a position and a bounding box. *)
type node_layout = {
  n_name : string;                      (** Dot label *)
  n_pos : pos;                        (** Center position *)
  n_bbox   : bounding_box;            (** Bounding box *)
  n_draw   : XDotDraw.operation list; (** Shape drawing *)
  n_ldraw  : XDotDraw.operation list; (** Label drawing *)
}


type cluster_layout = {
  c_pos : pos;
  c_bbox   : bounding_box;
  c_draw   : XDotDraw.operation list;
  c_ldraw  : XDotDraw.operation list;
}

type edge_layout = {
  e_draw   : XDotDraw.operation list; (** Shapes and curves *)
  e_ldraw  : XDotDraw.operation list; (** Label drawing *)
  e_hdraw  : XDotDraw.operation list; (** Head arrowhead drawing *)
  e_tdraw  : XDotDraw.operation list; (** Tail arrowhead drawing *)
  e_hldraw : XDotDraw.operation list; (** Head label drawing *)
  e_tldraw : XDotDraw.operation list; (** Tail label drawing *)
}

(** Creates a node layout *)
val mk_node_layout :
  name:string ->
  pos:pos ->
  bbox:bounding_box ->
  draw:XDotDraw.operation list ->
  ldraw:XDotDraw.operation list ->
  node_layout

(** Creates a cluster layout *)
val mk_cluster_layout :
  pos:pos ->
  bbox:bounding_box ->
  draw:XDotDraw.operation list ->
  ldraw:XDotDraw.operation list ->
  cluster_layout

(** Creates an edge layout *)
val mk_edge_layout :
  draw:XDotDraw.operation list ->
  ldraw:XDotDraw.operation list ->
  hdraw:XDotDraw.operation list ->
  tdraw:XDotDraw.operation list ->
  hldraw:XDotDraw.operation list ->
  tldraw:XDotDraw.operation list ->
  edge_layout

(** Parsing and reading XDot *)

exception ParseError of string

(** Instantiates a module which creates graph layouts from xdot files *)
module Make(G : Graph.Graphviz.GraphWithDotAttrs) : sig

  module HV: Hashtbl.S with type key = G.V.t
  module HE: Hashtbl.S with type key = G.E.t

  (** Main layout type *)
  type graph_layout =
    { vertex_layouts  : node_layout HV.t;
      edge_layouts    : edge_layout HE.t;
      cluster_layouts : (string, cluster_layout) Hashtbl.t;
      bbox : bounding_box }

  exception DotError of string

  (** Extracts a layout of an xdot file *)
  val layout_of_xdot: xdot_file:string -> G.t -> graph_layout

  (** Using the dot file and graphviz,
      create an xdot and extracts its layout. *)
  val layout_of_dot: ?cmd:string -> dot_file:string -> G.t -> graph_layout

end

(** Converts and reads various layout informations *)

(** [bounding_box pos w h] converts a bounding box of center [pos], 
    width [w] and height [h] from a Dot file to a pair of corners 
    (lower left and upper right) in the world coordinate system.
    @param pos position of the center of the node
    @param w width of the node
    @param h height of the node
*)
val bounding_box : (float * float) -> float -> float -> bounding_box

val read_bounding_box : string -> bounding_box

(** Reads xdot layouts from the dot ast *)
val read_node_layout : Dot_ast.node_id -> Dot_ast.attr list -> node_layout
val read_edge_layout : Dot_ast.attr list -> edge_layout
val read_cluster_layout : Dot_ast.attr list -> cluster_layout
