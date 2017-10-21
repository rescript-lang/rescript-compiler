(**************************************************************************)
(*                                                                        *)
(*  Ocamlgraph: a generic graph library for OCaml                         *)
(*  Copyright (C) 2004-2010                                               *)
(*  Sylvain Conchon, Jean-Christophe Filliatre and Julien Signoles        *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(** Interface with {i GraphViz}

    This module provides a basic interface with dot and neato,
    two programs of the GraphViz toolbox.
    These tools are available at the following URLs:
    - {v http://www.graphviz.org v}
    - {v http://www.research.att.com/sw/tools/graphviz v} *)

open Format

(***************************************************************************)
(** {2 Common stuff} *)

(** Because the neato and dot engines present a lot of common points -
    in particular in the graph description language, large parts of
    the code is shared.  The [CommonAttributes] module defines
    attributes of graphs, vertices and edges that are understood by the
    two engines.  Then module [DotAttributes] and [NeatoAttributes]
    define attributes specific to dot and neato respectively. *)

(*-------------------------------------------------------------------------*)
(** {3 Common types and signatures} *)

type color = int

type color_with_transparency = int32
(** The two least significant bytes encode the transparency information;
    the six most signification are the standard RGB color *)

val color_to_color_with_transparency: color -> color_with_transparency


type arrow_style =
  [ `None | `Normal | `Inv | `Dot | `Odot | `Invdot | `Invodot ]

(** The [ATTRIBUTES] module type defines the interface for the engines. *)
module type ATTRIBUTES = sig

  type graph  (** Attributes of graphs. *)

  type vertex (** Attributes of vertices. *)

  type edge   (** Attributes of edges. *)

  (** Attributes of (optional) boxes around vertices. *)
  type subgraph = {
    sg_name : string;            (** Box name. *)
    sg_attributes : vertex list; (** Box attributes. *)
    sg_parent : string option;   (** Nested subgraphs. *)
  }

end

(*-------------------------------------------------------------------------*)
(** {3 Common attributes} *)

(** The [CommonAttributes] module defines attributes for graphs, vertices and
    edges that are available in the two engines, dot and neato. *)
module CommonAttributes : sig

  (** Attributes of graphs. *)
  type graph =
    [ `Center of bool
    (** Centers the drawing on the page.  Default value is [false]. *)
    | `Fontcolor of color
    (** Sets the font color.  Default value is [black]. *)
    | `Fontname of string
    (** Sets the font family name.  Default value is ["Times-Roman"]. *)
    | `Fontsize of int
    (** Sets the type size (in points).  Default value is [14]. *)
    | `Label of string
    (** Caption for graph drawing. *)
    | `HtmlLabel of string
    (** Caption for graph drawing. In HTML strings, angle brackets must occur in
        matched pairs, and newlines and other formatting whitespace characters
        are allowed. In addition, the content must be legal XML, so that the
        special XML escape sequences for ", &, <, and > may be necessary in
        order to embed these characters in attribute values or raw text."  *)
    | `Orientation of [ `Portrait | `Landscape ]
    (** Sets the page orientation.  Default value is [`Portrait]. *)
    | `Page of float * float
    (** Sets the PostScript pagination unit, e.g [8.5, 11.0]. *)
    | `Pagedir of [ `TopToBottom | `LeftToRight ]
    (** Traversal order of pages.  Default value is [`TopToBottom]. *)
    | `Size of float * float
    (** Sets the bounding box of drawing (in inches). *)
    | `OrderingOut
      (** Constrains  order of out-edges in a subgraph according to
          their file sequence *)
    ]

  (** Attributes of vertices. *)
  type vertex =
    [ `Color of color
    (** Sets the color of the border of the vertex.
        Default value is [black] *)
    | `ColorWithTransparency of color_with_transparency
    (** Sets the color of the border of the vertex with a transparency
        component. Default value is fully opaque [black] *)
    | `Fontcolor of color
    (** Sets the label font color.  Default value is [black]. *)
    | `Fontname of string
    (** Sets the label font family name.  Default value is
        ["Times-Roman"]. *)
    | `Fontsize of int
    (** Sets the label type size (in points).  Default value is [14].
    *)
    | `Height of float
    (** Sets the minimum height.  Default value is [0.5]. *)
    | `Label of string
    (** Sets the label printed in the vertex.
        The string may include escaped
        newlines [\n], [\l], or [\r] for center, left, and right justified
        lines.
        Record labels may contain recursive box lists delimited by { | }.
    *)
    | `HtmlLabel of string
    (** Like label, in html style. In HTML strings, angle brackets must occur in
        matched pairs, and newlines and other formatting whitespace characters
        are allowed. In addition, the content must be legal XML, so that the
        special XML escape sequences for ", &, <, and > may be necessary in
        order to embed these characters in attribute values or raw text."  *)
    | `Orientation of float
    (** Vertex rotation angle, in degrees.  Default value is [0.0]. *)
    | `Penwidth of float
    (** Width of the pen (in points) used to draw the border of the node.
        Default value is [1.0]. *)
    | `Peripheries of int
    (** Sets  the  number  of periphery lines drawn around the polygon. *)
    | `Regular of bool
    (** If [true], then the polygon is made regular, i.e. symmetric about
        the x and y axis, otherwise  the polygon   takes   on   the  aspect
        ratio of the label.  Default value is [false]. *)
    | `Shape of
        [`Ellipse | `Box | `Circle | `Doublecircle | `Diamond
        | `Plaintext | `Record
        (* Addition through http://www.graphviz.org/doc/info/shapes.html *)
        | `Oval | `Egg | `Triangle | `Invtriangle
        | `Trapezium | `Invtrapezium
        | `House | `Invhouse
        | `Parallelogram | `Doubleoctagon | `Tripleoctagon
        | `Mdiamond | `Mcircle | `Msquare
        | `Star | `Underline
        | `Note | `Tab | `Folder
        | `Box3d | `Component  | `Promoter
        | `Cds
        | `Terminator | `Utr | `Primersite
        | `Restrictionsite
        | `Fivepoverhang | `Threepoverhang | `Noverhang
        | `Assembly | `Signature | `Insulator | `Ribosite | `Rnastab
        | `Proteasesite | `Proteinstab | `Rpromoter | `Rarrow
        | `Larrow | `Lpromoter
        (* Addition ends here *)
        | `Polygon of int * float]
    (** Sets the shape of the vertex.  Default value is [`Ellipse].
            [`Polygon (i, f)] draws a polygon with [n] sides and a skewing
            of [f]. *)
    | `Style of
        [ `Rounded | `Filled | `Solid | `Dashed | `Dotted | `Bold | `Invis ]
    (** Sets the layout style of the vertex.
        Several styles may be combined simultaneously. *)
    | `Width of float
      (** Sets the minimum width.  Default value is [0.75]. *)
    ]

  (** Attributes of edges. *)
  type edge =
    [ `Color of color
    (** Sets the edge stroke color.  Default value is [black]. *)
    | `ColorWithTransparency of color_with_transparency
    (** Sets the edge stroke color with a transparency
        component. Default value is fully opaque [black] *)
    | `Decorate of bool
    (** If [true], draws a line connecting labels with their edges. *)
    | `Dir of [ `Forward | `Back | `Both | `None ]
    (** Sets arrow direction.  Default value is [`Forward]. *)
    | `Fontcolor of color
    (** Sets the label font color.  Default value is [black]. *)
    | `Fontname of string
    (** Sets the label font family name.  Default value is
        ["Times-Roman"]. *)
    | `Fontsize of int
    (** Sets the label type size (in points).  Default value is [14]. *)
    | `Label of string
    (** Sets the label to be attached to the edge.  The string may include
        escaped newlines [\n], [\l], or [\r] for centered, left, or right
        justified lines. *)
    | `HtmlLabel of string
    (** Like label, in html style. In HTML strings, angle brackets must occur in
        matched pairs, and newlines and other formatting whitespace characters
        are allowed. In addition, the content must be legal XML, so that the
        special XML escape sequences for ", &, <, and > may be necessary in
        order to embed these characters in attribute values or raw text."  *)
    | `Labelfontcolor of color
    (** Sets the font color for head and tail labels.  Default value is
        [black]. *)
    | `Labelfontname of string
    (** Sets the font family name for head and tail labels.  Default
        value is ["Times-Roman"]. *)
    | `Labelfontsize of int
    (** Sets the font size for head and tail labels (in points).
        Default value is [14]. *)
    | `Penwidth of float
    (** Width of the pen (in points) used to draw the edge. Default value
        is [1.0].  *)
    | `Style of [ `Solid | `Dashed | `Dotted | `Bold | `Invis ]
      (** Sets the layout style of the edge.  Several styles may be combined
          simultaneously. *)
    ]

end

(***************************************************************************)
(** {2 Interface with the dot engine} *)

(** [DotAttributes] extends [CommonAttributes] and implements [ATTRIBUTES]. *)
module DotAttributes : sig

  (** Attributes of graphs.  They include all common graph attributes and
      several specific ones.  All attributes described in the "dot User's
      Manual, February 4, 2002" are handled, excepted: clusterank, color,
      compound, labeljust, labelloc, ordering, rank, remincross, rotate,
      searchsize and style. *)
  type graph =
    [ CommonAttributes.graph
    | `Bgcolor of color
    (** Sets the background color and the inital fill color. *)
    | `BgcolorWithTransparency of color_with_transparency
    (** Sets the background color and the inital fill color with
        a transparency component. *)
    | `Comment of string
    (** Comment string. *)
    | `Concentrate of bool
    (** If [true], enables edge concentrators.  Default value is [false]. *)
    | `Fontpath of string
    (** List of directories for fonts. *)
    | `Layers of string list
    (** List of layers. *)
    | `Margin of float
    (** Sets the page margin (included in the page size).  Default value is
        [0.5]. *)
    | `Mclimit of float
    (** Scale factor for mincross iterations.  Default value is [1.0]. *)
    | `Nodesep of float
    (** Sets the minimum separation between nodes, in inches.  Default
        value is [0.25]. *)
    | `Nslimit of int
    (** If set of [f], bounds network simplex iterations by [f *
        <number of nodes>] when ranking nodes. *)
    | `Nslimit1 of int
    (** If set of [f], bounds network simplex iterations by [f *
        <number of nodes>] when setting x-coordinates. *)
    | `Ranksep of float
    (** Sets the minimum separation between ranks. *)
    | `Quantum of float
    (** If not [0.0], node label dimensions will be rounded to integral
        multiples of it.  Default value is [0.0]. *)
    | `Rankdir of [ `TopToBottom | `LeftToRight ]
    (** Direction of rank ordering.  Default value is [`TopToBottom]. *)
    | `Ratio of [ `Float of float | `Fill | `Compress| `Auto ]
    (** Sets the aspect ratio. *)
    | `Samplepoints of int
    (** Number of points used to represent ellipses and circles on output.
        Default value is [8]. *)
    | `Url of string
      (** URL associated with graph (format-dependent). *)
    ]

  (** Attributes of nodes.  They include all common node attributes and
      several specific ones.  All attributes described in the "dot User's
      Manual, February 4, 2002" are handled, excepted: bottomlabel, group,
      shapefile and toplabel. *)
  type vertex =
    [ CommonAttributes.vertex
    | `Comment of string
    (** Comment string. *)
    | `Distortion of float
    (* TEMPORARY *)
    | `Fillcolor of color
    (** Sets the fill color (used when `Style filled).  Default value
        is [lightgrey]. *)
    | `FillcolorWithTransparency of color_with_transparency
    (** Sets the fill color (used when `Style filled) with a transparency
        component.  Default value is fully opaque [lightgrey]. *)
    | `Fixedsize of bool
    (** If [true], forces the given dimensions to be the actual ones.
        Default value is [false]. *)
    | `Layer of string
    (** Overlay. *)
    | `Url of string
    (** The  default  url  for  image  map  files; in PostScript files,
        the base URL for all relative URLs, as recognized by Acrobat
        Distiller 3.0 and up. *)
    | `Z of float
      (** z coordinate for VRML output. *)
    ]

  (** Attributes of edges.  They include all common edge attributes and
      several specific ones.  All attributes described in the "dot User's
      Manual, February 4, 2002" are handled, excepted: lhead and ltail. *)
  type edge =
    [ CommonAttributes.edge
    | `Arrowhead of arrow_style
    (** Sets the style of the head arrow.  Default value is [`Normal]. *)
    | `Arrowsize of float
    (** Sets the scaling factor of arrowheads.  Default value is [1.0]. *)
    | `Arrowtail of arrow_style
    (** Sets the style of the tail arrow.  Default value is [`Normal]. *)
    | `Comment of string
    (** Comment string. *)
    | `Constraint of bool
    (** If [false], causes an edge to be ignored for rank assignment.
        Default value is [true]. *)
    | `Headlabel of string
    (** Sets the label attached to the head arrow. *)
    | `Headport of [ `N | `NE | `E | `SE | `S | `SW | `W | `NW ]
    (* TEMPORARY *)
    | `Headurl of string
    (** Url attached to head label if output format is ismap. *)
    | `Labelangle of float
    (** Angle in degrees which head or tail label is rotated off edge.
        Default value is [-25.0]. *)
    | `Labeldistance of float
    (** Scaling factor for distance of head or tail label from node.
        Default value is [1.0]. *)
    | `Labelfloat of bool
    (** If [true], lessen constraints on edge label placement.
        Default value is [false]. *)
    | `Layer of string
    (** Overlay. *)
    | `Minlen of int
    (** Minimum rank distance between head an tail.
        Default value is [1]. *)
    | `Samehead of string
    (** Tag for head node; edge heads with the same tag are merged onto the
        same port. *)
    | `Sametail of string
    (** Tag for tail node; edge tails with the same tag are merged onto the
        same port. *)
    | `Taillabel of string
    (** Sets the label attached to the tail arrow. *)
    | `Tailport of [ `N | `NE | `E | `SE | `S | `SW | `W | `NW ]
    (* TEMPORARY *)
    | `Tailurl of string
    (** Url attached to tail label if output format is ismap. *)
    | `Weight of int
      (** Sets the integer cost of stretching the edge.  Default value is
          [1]. *)
    ]

  (** Subgraphs have a name and some vertices. *)
  type subgraph = {
    sg_name : string;
    sg_attributes : vertex list;
    sg_parent : string option;
  }

end

(** Graph module with dot attributes *)
module type GraphWithDotAttrs = sig

  include Sig.G

  val graph_attributes: t -> DotAttributes.graph list

  (** Vertex attributes *)

  val default_vertex_attributes: t -> DotAttributes.vertex list
  val vertex_name : V.t -> string
  val vertex_attributes: V.t -> DotAttributes.vertex list

  (** Edge attributes *)

  val default_edge_attributes: t -> DotAttributes.edge list
  val edge_attributes: E.t -> DotAttributes.edge list

  val get_subgraph : V.t -> DotAttributes.subgraph option
  (** The box (if exists) which the vertex belongs to. Boxes with same
         names are not distinguished and so they should have the same
         attributes. *)

end

module Dot
    (X : sig

       (** Graph implementation. Sub-signature of {!Sig.G} *)

       type t
       module V : sig type t end
       module E : sig type t val src : t -> V.t val dst : t -> V.t end
       val iter_vertex : (V.t -> unit) -> t -> unit
       val iter_edges_e : (E.t -> unit) -> t -> unit

       (** Graph, vertex and edge attributes. *)

       val graph_attributes: t -> DotAttributes.graph list

       val default_vertex_attributes: t -> DotAttributes.vertex list
       val vertex_name : V.t -> string
       val vertex_attributes: V.t -> DotAttributes.vertex list

       val get_subgraph : V.t -> DotAttributes.subgraph option
       (** The box (if exists) which the vertex belongs to. Boxes with same
           names are not distinguished and so they should have the same
           attributes. *)

       val default_edge_attributes: t -> DotAttributes.edge list
       val edge_attributes: E.t -> DotAttributes.edge list

     end) :
sig

  val fprint_graph: formatter -> X.t -> unit
  (** [fprint_graph ppf graph] pretty prints the graph [graph] in
      the CGL language on the formatter [ppf]. *)

  val output_graph: out_channel -> X.t -> unit
  (** [output_graph oc graph] pretty prints the graph [graph] in the dot
      language on the channel [oc]. *)

end

(***************************************************************************)
(** {2 The neato engine} *)

module NeatoAttributes : sig

  (** Attributes of graphs.  They include all common graph attributes and
      several specific ones.  All attributes described in the "Neato User's
      manual, April 10, 2002" are handled. *)
  type graph =
    [ CommonAttributes.graph
    | `Margin of float * float
    (** Sets the page margin (included in the page size).  Default value is
        [0.5, 0.5]. *)
    | `Start of int
    (** Seed for random number generator. *)
    | `Overlap of bool
    (** Default value is [true]. *)
    | `Spline of bool
    (** [true] makes edge splines if nodes don't overlap.
        Default value is [false]. *)
    | `Sep of float
      (** Edge spline separation factor from nodes.  Default value
          is [0.0]. *)
    ]

  (** Attributes of nodes.  They include all common node attributes and
      several specific ones.  All attributes described in the "Neato User's
      manual, April 10, 2002" are handled. *)
  type vertex =
    [ CommonAttributes.vertex
    | `Pos of float * float
      (** Initial coordinates of the vertex. *)
    ]

  (** Attributes of edges.  They include all common edge attributes and
      several specific ones.  All attributes described in the "Neato User's
      manual, April 10, 2002" are handled. *)
  type edge =
    [ CommonAttributes.edge
    | `Id of string
    (** Optional value to distinguish multiple edges. *)
    | `Len of float
    (** Preferred length of edge.  Default value is [1.0]. *)
    | `Weight of float
      (** Strength of edge spring.  Default value is [1.0]. *)
    ]

  (** Subgraphs have a name and some vertices. *)
  type subgraph = {
    sg_name : string;
    sg_attributes : vertex list;
    sg_parent : string option;
  }

end

module Neato
    (X : sig

       (** Graph implementation. Sub-signature of {!Sig.G}. *)

       type t
       module V : sig type t end
       module E : sig type t val src : t -> V.t val dst : t -> V.t end

       val iter_vertex : (V.t -> unit) -> t -> unit
       val iter_edges_e : (E.t -> unit) -> t -> unit

       (** Graph, vertex and edge attributes. *)

       val graph_attributes: t -> NeatoAttributes.graph list

       val default_vertex_attributes: t -> NeatoAttributes.vertex list
       val vertex_name : V.t -> string
       val vertex_attributes: V.t -> NeatoAttributes.vertex list

       val get_subgraph : V.t -> NeatoAttributes.subgraph option
       (** The box (if exists) which the vertex belongs to. Boxes with same
           names are not distinguished and so they should have the same
           attributes. *)

       val default_edge_attributes: t -> NeatoAttributes.edge list
       val edge_attributes: E.t -> NeatoAttributes.edge list

     end) :
sig

  val set_command: string -> unit
  (** Several functions provided by this module run the external program
      {i neato}.  By default, this command is supposed to be in the default
      path and is invoked by {i neato}.  The function
      [set_command] allows to set an alternative path at run time. *)

  exception Error of string
  val handle_error: ('a -> 'b) -> 'a -> 'b

  val fprint_graph: formatter -> X.t -> unit
  (** [fprint_graph ppf graph] pretty prints the graph [graph] in
      the CGL language on the formatter [ppf]. *)

  val output_graph: out_channel -> X.t -> unit
  (** [output_graph oc graph] pretty prints the graph [graph] in the dot
      language on the channel [oc]. *)

end

(*
Local Variables:
compile-command: "make -C .."
End:
*)
