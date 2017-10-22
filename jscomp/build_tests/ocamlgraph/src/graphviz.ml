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
      http://www.graphviz.org/
      http://www.research.att.com/sw/tools/graphviz/ *)

open Format
open Pervasives (* for compatibility with ocaml 3.12.0+dev17
                   (incoming ocaml3.12) *)

(***************************************************************************)
(** {2 Common stuff} *)

(** Because the neato and dot engines present a lot of common points -
    in particular in the graph description language, large parts of
    the code is shared.  First, the [!CommonAttributes] module defines
    attributes of graphs, nodes and edges that are understood by the
    two engines.  Second, given a module (of type [!ENGINE])
    describing an engine the [!MakeEngine] functor provides suitable
    interface function for it. *)

(*-------------------------------------------------------------------------*)
(** {3 Common attributes} *)

type color = int

type color_with_transparency = int32

let color_to_color_with_transparency color =
  Int32.add (Int32.shift_left (Int32.of_int color) 8) 0xFFl

let fprint_color ppf color =
  fprintf ppf "\"#%06X\"" color

let fprint_color_with_transparency ppf color =
  fprintf ppf "\"#%08lX\"" color

let fprint_string ppf s = fprintf ppf "\"%s\"" s
(*  let s' = String.escaped s in
    if s' = s && s <> ""
    then fprintf ppf "%s" s
    else fprintf ppf "\"%s\"" s'*)

let fprint_string_user ppf s =
  (*  let s = String.escaped s in*)
  fprintf ppf "\"%s\"" s

let fprint_htmlstring_user ppf s = fprintf ppf "<%s>" s

let fprint_square_not_empty printer ppf = function
  | [] -> ()
  | l -> fprintf ppf " [%a]" printer l

type arrow_style =
  [ `None | `Normal | `Inv | `Dot | `Odot | `Invdot | `Invodot ]

let fprint_arrow_style ppf = function
    `None -> fprintf ppf "none"
  | `Normal -> fprintf ppf "normal"
  | `Inv -> fprintf ppf "inv"
  | `Dot -> fprintf ppf "dot"
  | `Odot -> fprintf ppf "odot"
  | `Invdot -> fprintf ppf "invdot"
  | `Invodot -> fprintf ppf "invodot"

let fprint_dir ppf = function
    `TopToBottom -> fprintf ppf "TB"
  | `LeftToRight -> fprintf ppf "LR"

type symbseq =
  | COMMA
  | SEMI

let fprint_symbseq ppf = function
  | COMMA -> pp_print_string ppf ","
  | SEMI  -> pp_print_string ppf ";"

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

(** The [CommonAttributes] module defines attributes for graphs, nodes and
    edges that are available in the two engines, dot and neato. *)
module CommonAttributes = struct

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

  (** Attributes of nodes. *)
  type vertex =
    [ `Color of color
    (** Sets the color of the border of the node. Default value is [black]
    *)
    | `ColorWithTransparency of color_with_transparency
    (** Sets the color of the border of the vertex with a transparency
        component. Default value is fully opaque [black] *)
    | `Fontcolor of color
    (** Sets the label font color.  Default value is [black]. *)
    | `Fontname of string
    (** Sets the label font family name.  Default value is
        ["Times-Roman"]. *)
    | `Fontsize of int
    (** Sets the label type size (in points).  Default value is [14]. *)
    | `Height of float
    (** Sets the minimum height.  Default value is [0.5]. *)
    | `Label of string
    (** Sets the label printed in the node. The string may include escaped
        newlines [\n], [\l], or [\r] for center, left, and right justified
        lines.
        Record labels may contain recursive box lists delimited by { | }.
    *)
    | `HtmlLabel of string
    | `Orientation of float
    (** Node rotation angle, in degrees.  Default value is [0.0]. *)
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
        | `Plaintext | `Record | `Polygon of int * float]
    (** Sets the shape of the node.  Default value is [`Ellipse].
        [`Polygon (i, f)] draws a polygon with [n] sides and a skewing
        of [f]. *)
    | `Style of
        [ `Rounded | `Filled | `Solid | `Dashed | `Dotted | `Bold | `Invis ]
    (** Sets the layout style of the node.  Several styles may be combined
        simultaneously. *)
    | `Width of float
      (** Sets the minimum width.  Default value is [0.75]. *)
    ]

  (** Attributes of edges. *)
  type edge =
    [ `Color of color
    (** Sets the edge stroke color.  Default value is [black]. *)
    | `ColorWithTransparency of color_with_transparency
    (** Sets the color of the border of the vertex with a transparency
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

  (** Pretty-print. *)

  let fprint_orientation ppf = function
      `Portrait -> fprintf ppf "portrait"
    | `Landscape -> fprintf ppf "landscape"

  let fprint_graph ppf = function
      `Center b -> fprintf ppf "center=%i" (if b then 1 else 0)
    | `Fontcolor a -> fprintf ppf "fontcolor=%a" fprint_color a
    | `Fontname s -> fprintf ppf "fontname=%a" fprint_string s
    | `Fontsize i -> fprintf ppf "fontsize=%i" i
    | `Label s -> fprintf ppf "label=%a" fprint_string_user s
    | `HtmlLabel s -> fprintf ppf "label=%a" fprint_htmlstring_user s
    | `Orientation a -> fprintf ppf "orientation=%a" fprint_orientation a
    | `Page (x, y) -> fprintf ppf "page=\"%f,%f\"" x y
    | `Pagedir a -> fprintf ppf "pagedir=%a" fprint_dir a
    | `Size (x, y) -> fprintf ppf "size=\"%f,%f\"" x y
    | `OrderingOut -> fprintf ppf "ordering=out"

  let fprint_shape ppf = function
    | `Ellipse -> fprintf ppf "ellipse"
    | `Box -> fprintf ppf "box"
    | `Circle -> fprintf ppf "circle"
    | `Doublecircle -> fprintf ppf "doublecircle"
    | `Diamond -> fprintf ppf "diamond"
    | `Plaintext -> fprintf ppf "plaintext"
    | `Record -> fprintf ppf "record"
    | `Egg -> fprintf ppf "egg"
    | `House -> fprintf ppf "house"
    | `Invhouse -> fprintf ppf "invhouse"
    | `Trapezium ->  fprintf ppf "trapezium"
    | `Invtrapezium ->  fprintf ppf "invtrapezium"
    | `Triangle -> fprintf ppf "triangle"
    | `Invtriangle -> fprintf ppf "invtriangle"
    | `Oval ->  fprintf ppf "oval"
    | `Assembly -> fprintf ppf "assembly"
    | `Box3d -> fprintf ppf "box3d"
    | `Cds -> fprintf ppf "cds"
    | `Component -> fprintf ppf "component"
    | `Doubleoctagon -> fprintf ppf "doubleoctagon"
    | `Fivepoverhang -> fprintf ppf "fivepoverhang"
    | `Folder -> fprintf ppf "folder"
    | `Insulator -> fprintf ppf "insulator"
    | `Larrow -> fprintf ppf "larrow"
    | `Lpromoter -> fprintf ppf "lpromoter"
    | `Mcircle -> fprintf ppf "mcircle"
    | `Mdiamond -> fprintf ppf "mdiamond"
    | `Msquare -> fprintf ppf " msquare"
    | `Note -> fprintf ppf "note"
    | `Noverhang -> fprintf ppf "noverhang"
    | `Parallelogram -> fprintf ppf "parallelogram"
    | `Primersite -> fprintf ppf "primersite"
    | `Promoter -> fprintf ppf "promoter"
    | `Proteasesite -> fprintf ppf "proteasesite"
    | `Proteinstab -> fprintf ppf "proteinstab"
    | `Rarrow -> fprintf ppf "rarrow"
    | `Restrictionsite -> fprintf ppf "restrictionsite"
    | `Ribosite -> fprintf ppf "ribosite"
    | `Rnastab -> fprintf ppf "rnastab"
    | `Rpromoter -> fprintf ppf "rpromoter"
    | `Signature -> fprintf ppf "signature"
    | `Star -> fprintf ppf "star"
    | `Tab -> fprintf ppf "tab"
    | `Terminator -> fprintf ppf "terminator"
    | `Threepoverhang -> fprintf ppf "threepoverhang"
    | `Tripleoctagon -> fprintf ppf "tripleoctagon"
    | `Underline -> fprintf ppf "underline"
    | `Utr -> fprintf ppf "utr"
    | `Polygon (i, f) -> fprintf ppf "polygon, sides=%i, skew=%f" i f

  let rec fprint_string_list ppf = function
    | [] -> ()
    | [hd] -> fprintf ppf "%s" hd
    | hd :: tl -> fprintf ppf "%s,%a" hd fprint_string_list tl

  let node_style_str = function
    | `Rounded -> "rounded"
    | `Filled -> "filled"
    | `Solid -> "solid"
    | `Dashed -> "dashed"
    | `Dotted -> "dotted"
    | `Bold -> "bold"
    | `Invis -> "invis"

  let fprint_style_list sep ppf a =
    fprintf ppf "style=\"%a\"%a@ "
      fprint_string_list (List.map node_style_str a)
      fprint_symbseq sep

  let fprint_vertex ppf = function
    | `Color a -> fprintf ppf "color=%a" fprint_color a
    | `ColorWithTransparency a ->
      fprintf ppf "color=%a" fprint_color_with_transparency a
    | `Fontcolor a -> fprintf ppf "fontcolor=%a" fprint_color a
    | `Fontname s -> fprintf ppf "fontname=%a"  fprint_string s
    | `Fontsize i -> fprintf ppf "fontsize=%i" i
    | `Height f -> fprintf ppf "height=%f" f
    | `Label s -> fprintf ppf "label=%a" fprint_string_user s
    | `HtmlLabel s -> fprintf ppf "label=%a" fprint_htmlstring_user s
    | `Orientation f -> fprintf ppf "orientation=%f" f
    | `Penwidth f -> fprintf ppf "penwidth=%f" f
    | `Peripheries i -> fprintf ppf "peripheries=%i" i
    | `Regular b -> fprintf ppf "regular=%b" b
    | `Shape a -> fprintf ppf "shape=%a" fprint_shape a
    | `Style _ -> assert false
    | `Width f -> fprintf ppf "width=%f" f

  let fprint_arrow_direction ppf = function
      `Forward -> fprintf ppf "forward"
    | `Back -> fprintf ppf "back"
    | `Both -> fprintf ppf "both"
    | `None -> fprintf ppf "none"

  let fprint_edge ppf = function
    | `Color a -> fprintf ppf "color=%a" fprint_color a
    | `ColorWithTransparency a ->
      fprintf ppf "color=%a" fprint_color_with_transparency a
    | `Decorate b -> fprintf ppf "decorate=%b" b
    | `Dir a -> fprintf ppf "dir=%a" fprint_arrow_direction a
    | `Fontcolor a -> fprintf ppf "fontcolor=%a" fprint_color a
    | `Fontname s -> fprintf ppf "fontname=%a" fprint_string s
    | `Fontsize i -> fprintf ppf "fontsize=%i" i
    | `Label s -> fprintf ppf "label=%a" fprint_string_user s
    | `HtmlLabel s -> fprintf ppf "label=%a" fprint_htmlstring_user s
    | `Labelfontcolor a -> fprintf ppf "labelfontcolor=%a" fprint_color a
    | `Labelfontname s -> fprintf ppf "labelfontname=\"%s\"" s
    (* (String.escaped s) *)
    | `Labelfontsize i -> fprintf ppf "labelfontsize=%i" i
    | `Penwidth f -> fprintf ppf "penwidth=%f" f
    | `Style _ -> assert false

  let rec filter_style al sl l = match l with
    | [] -> al, sl
    | `Style s :: l -> filter_style al (s :: sl) l
    | a :: l -> filter_style (a :: al) sl l

  (** [fprint_graph_attribute printer ppf list] pretty prints a list of
      attributes on the formatter [ppf], using the printer [printer] for
      each attribute.  The list appears between brackets and attributes
      are speparated by ",".  If the list is empty, nothing is printed. *)
  let fprint_attributes fprint_style_list fprint_attribute sep ppf list =
    if list <> [] then begin
      let list, styles = filter_style [] [] list in
      let rec fprint_attributes_rec ppf = function
        | [] -> ()
        | hd :: tl ->
          fprintf ppf "%a%a@ "
            fprint_attribute hd
            fprint_symbseq sep;
          fprint_attributes_rec ppf tl
      in
      fprintf ppf "@[<hov>%a" fprint_attributes_rec list;
      if styles <> [] then begin
        fprint_style_list sep ppf styles
      end;
      fprintf ppf "@]"
    end

end


(*-------------------------------------------------------------------------*)
(** {3 The [MakeEngine] functor} *)

(** An engine is described by a module of the following signature. *)
module type ENGINE = sig

  module Attributes : sig
    include ATTRIBUTES
    val fprint_graph:formatter -> graph -> unit
    val fprint_vertex_list: symbseq -> formatter -> vertex list -> unit
    val fprint_edge_list: symbseq -> formatter -> edge list -> unit
  end

  (** The litteral name of the engine. *)
  val name: string

  (** The keyword for graphs ("digraph" for dot, "graph" for neato) *)
  val opening: string

  (** The litteral for edge arrows ("->" for dot, "--" for neato) *)
  val edge_arrow: string

end

module type GRAPH = sig

end

module MakeEngine
    (EN: ENGINE)
    (X : sig
       type t
       module V : sig type t end
       module E : sig type t val src : t -> V.t val dst : t -> V.t end

       val iter_vertex : (V.t -> unit) -> t -> unit
       val iter_edges_e : (E.t -> unit) -> t -> unit

       val graph_attributes: t -> EN.Attributes.graph list

       val default_vertex_attributes: t -> EN.Attributes.vertex list
       val vertex_name : V.t -> string
       val vertex_attributes: V.t -> EN.Attributes.vertex list

       val default_edge_attributes: t -> EN.Attributes.edge list
       val edge_attributes: E.t -> EN.Attributes.edge list
       val get_subgraph : V.t -> EN.Attributes.subgraph option
     end) =
struct

  let command = ref EN.name
  let set_command cmd =
    command := cmd

  exception Error of string

  let handle_error f arg =
    try
      f arg
    with
      Error msg ->
      Printf.eprintf "%s: %s failure\n   %s\n"
        Sys.argv.(0) EN.name msg;
      flush stderr;
      exit 2

  (** [fprint_graph_attributes ppf list] pretty prints a list of
      graph attributes on the formatter [ppf].  Attributes are separated
      by a ";". *)
  let fprint_graph_attributes ppf list =
    List.iter (function att ->
        fprintf ppf "%a;@ " EN.Attributes.fprint_graph att
      ) list

  (** [fprint_graph ppf graph] pretty prints the graph [graph] in
      the CGL language on the formatter [ppf]. *)
  let fprint_graph ppf graph =
    let module SG = Map.Make(String) in
    let subgraphs = ref SG.empty in

    (* Printing nodes. *)

    let print_nodes ppf =
      let default_node_attributes = X.default_vertex_attributes graph in
      if default_node_attributes  <> [] then
        fprintf ppf "node%a;@ "
          (fprint_square_not_empty (EN.Attributes.fprint_vertex_list COMMA))
          default_node_attributes;

      X.iter_vertex
        (function node ->
          begin match X.get_subgraph node with
            | None -> ()
            | Some sg ->
              let (sg, nodes) =
                if SG.mem sg.EN.Attributes.sg_name !subgraphs then
                  SG.find sg.EN.Attributes.sg_name !subgraphs
                else
                  (sg, [])
              in
              subgraphs := SG.add sg.EN.Attributes.sg_name (sg, node :: nodes) !subgraphs
          end;
          fprintf ppf "%s%a;@ "
            (X.vertex_name node)
            (fprint_square_not_empty (EN.Attributes.fprint_vertex_list COMMA))
            (X.vertex_attributes node)
        )
        graph
    in

    (* Printing subgraphs *)

    let rec print_nested_subgraphs ppf = function
      | [] ->
        () (* no more work to do, so terminate *)
      | name :: worklist ->
        let sg, nodes = SG.find name !subgraphs in
        let children = SG.filter (fun _ (sg, _) -> sg.EN.Attributes.sg_parent = Some name) !subgraphs in
        fprintf ppf "@[<v 2>subgraph cluster_%s { %a%t@ %t };@]@\n"

          name

          (EN.Attributes.fprint_vertex_list SEMI)
          sg.EN.Attributes.sg_attributes

          (fun ppf ->
             (List.iter (fun n -> fprintf ppf "%s;" (X.vertex_name n)) nodes)
          )

          (fun ppf ->
             print_nested_subgraphs ppf (List.map fst (SG.bindings children))
          );

        print_nested_subgraphs ppf worklist
    in

    let print_subgraphs ppf =
      let root_worklist = SG.filter (fun _ (sg, _) -> sg.EN.Attributes.sg_parent = None) !subgraphs in
      print_nested_subgraphs ppf (List.map fst (SG.bindings root_worklist))
    in

    (* Printing edges *)

    let print_edges ppf =

      let default_edge_attributes = X.default_edge_attributes graph in
      if default_edge_attributes <> [] then
        fprintf ppf "edge%a;@ "
          (fprint_square_not_empty (EN.Attributes.fprint_edge_list COMMA))
          default_edge_attributes;

      X.iter_edges_e (function edge ->
          fprintf ppf "%s %s %s%a;@ "
            (X.vertex_name (X.E.src edge))
            EN.edge_arrow
            (X.vertex_name (X.E.dst edge))
            (fprint_square_not_empty (EN.Attributes.fprint_edge_list COMMA))
            (X.edge_attributes edge)
        ) graph

    in

    fprintf ppf "@[<v>%s G {@ @[<v 2>  %a"
      EN.opening
      fprint_graph_attributes (X.graph_attributes graph);
    fprintf ppf "%t@ " print_nodes;
    fprintf ppf "%t@ " print_subgraphs;
    fprintf ppf "%t@ " print_edges;
    fprintf ppf "@]}@]"

  (** [output_graph oc graph] pretty prints the graph [graph] in the dot
      language on the channel [oc]. *)
  let output_graph oc graph =
    let ppf = formatter_of_out_channel oc in
    fprint_graph ppf graph;
    pp_print_flush ppf ()

end

(***************************************************************************)
(** {2 Interface with the dot engine} *)

(** The [DotAttributes] module defines attributes for graphs, nodes and edges
    that are available in the dot engine. *)
module DotAttributes = struct

  (** Attributes of graphs.  They include all common graph attributes and
      several specific ones.  All attributes described in the "dot User's
      Manual, February 4, 2002" are handled, excepted: clusterank, color,
      compound, labeljust, labelloc, ordering, rank, remincross, rotate,
      searchsize and style.
  *)
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
      shapefile and toplabel.
  *)
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
      Manual, February 4, 2002" are handled, excepted: lhead and ltail.
  *)
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
    (** Minimum rank distance between head an tail.  Default value is [1]. *)
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

  type subgraph = {
    sg_name : string;
    sg_attributes : vertex list;
    sg_parent : string option;
  }

  (** {4 Pretty-print of attributes} *)

  let rec fprint_string_list ppf = function
      [] -> ()
    | [hd] -> fprintf ppf "%s" hd
    | hd :: tl -> fprintf ppf "%s,%a" hd fprint_string_list tl

  let fprint_ratio ppf = function
      `Float f -> fprintf ppf "%f" f
    | `Fill -> fprintf ppf "fill"
    | `Compress -> fprintf ppf "compress"
    | `Auto -> fprintf ppf "auto"

  let fprint_graph ppf = function
    #CommonAttributes.graph as att -> CommonAttributes.fprint_graph ppf att
    | `Bgcolor a -> fprintf ppf "bgcolor=%a" fprint_color a
    | `BgcolorWithTransparency a ->
      fprintf ppf "bgcolor=%a" fprint_color_with_transparency a
    | `Comment s -> fprintf ppf "comment=%a" fprint_string s
    | `Concentrate b -> fprintf ppf "concentrate=%b" b
    | `Fontpath s -> fprintf ppf "fontpath=%a" fprint_string s
    | `Layers s -> fprintf ppf "layers=%a" fprint_string_list s
    | `Margin f -> fprintf ppf "margin=%f" f
    | `Mclimit f -> fprintf ppf "mclimit=%f" f
    | `Nodesep f -> fprintf ppf "nodesep=%f" f
    | `Nslimit i -> fprintf ppf "nslimit=%i" i
    | `Nslimit1 i -> fprintf ppf "nslimit1=%i" i
    | `Ranksep f -> fprintf ppf "ranksep=%f" f
    | `Quantum f -> fprintf ppf "quantum=%f" f
    | `Rankdir a -> fprintf ppf "rankdir=%a" fprint_dir a
    | `Ratio a -> fprintf ppf "ratio=%a" fprint_ratio a
    | `Samplepoints i -> fprintf ppf "samplepoints=%i" i
    | `Url s -> fprintf ppf "URL=\"%s\"" s (*(String.escaped s)*)

  let fprint_vertex ppf = function
    #CommonAttributes.vertex as att ->
    CommonAttributes.fprint_vertex ppf att
    | `Comment s -> fprintf ppf "comment=%a" fprint_string s
    | `Distortion f -> fprintf ppf "distortion=%f" f
    | `Fillcolor a -> fprintf ppf "fillcolor=%a" fprint_color a
    | `FillcolorWithTransparency a ->
      fprintf ppf "fillcolor=%a" fprint_color_with_transparency a
    | `Fixedsize b -> fprintf ppf "fixedsize=%b" b
    | `Layer s -> fprintf ppf "layer=%a" fprint_string s
    | `Url s -> fprintf ppf "URL=\"%s\"" s (*(String.escaped s)*)
    | `Z f -> fprintf ppf "z=%f" f

  let fprint_port ppf = function
      `N -> fprintf ppf "n"
    | `NE -> fprintf ppf "ne"
    | `E -> fprintf ppf "e"
    | `SE -> fprintf ppf "se"
    | `S -> fprintf ppf "s"
    | `SW -> fprintf ppf "sw"
    | `W -> fprintf ppf "w"
    | `NW -> fprintf ppf "nw"

  let fprint_edge ppf = function
    #CommonAttributes.edge as att -> CommonAttributes.fprint_edge ppf att
    | `Arrowhead a -> fprintf ppf "arrowhead=%a" fprint_arrow_style a
    | `Arrowsize f -> fprintf ppf "arrowsize=%f" f
    | `Arrowtail a -> fprintf ppf "arrowtail=%a" fprint_arrow_style a
    | `Comment s -> fprintf ppf "comment=%a" fprint_string s
    | `Constraint b -> fprintf ppf "constraint=%b" b
    | `Headlabel s -> fprintf ppf "headlabel=%a" fprint_string s
    | `Headport a -> fprintf ppf "headport=%a" fprint_port a
    | `Headurl s -> fprintf ppf "headURL=%a" fprint_string s
    | `Labelangle f -> fprintf ppf "labelangle=%f" f
    | `Labeldistance f -> fprintf ppf "labeldistance=%f" f
    | `Labelfloat b -> fprintf ppf "labelfloat=%b" b
    | `Layer s -> fprintf ppf "layer=%a" fprint_string s
    | `Minlen i -> fprintf ppf "minlen=%i" i
    | `Samehead s -> fprintf ppf "samehead=%a" fprint_string s
    | `Sametail s -> fprintf ppf "sametail=%a" fprint_string s
    | `Taillabel s -> fprintf ppf "taillabel=%a" fprint_string s
    | `Tailport a -> fprintf ppf "tailport=%a" fprint_port a
    | `Tailurl s -> fprintf ppf "tailURL=%a" fprint_string s
    | `Weight i -> fprintf ppf "weight=%i" i

  let fprint_vertex_list =
    CommonAttributes.fprint_attributes
      CommonAttributes.fprint_style_list fprint_vertex

  let fprint_edge_list =
    CommonAttributes.fprint_attributes
      CommonAttributes.fprint_style_list fprint_edge

end

(** Graph modules with dot attributes *)
module type GraphWithDotAttrs = sig
  include Sig.G

  (** Graph, vertex and edge attributes. *)
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

module Dot =
  MakeEngine (struct
    module Attributes = DotAttributes
    let name = "dot"
    let opening = "digraph"
    let edge_arrow = "->"
  end)

(***************************************************************************)
(** {2 Interface with the neato engine} *)

(** The [NeatoAttributes] module defines attributes for graphs, nodes and edges
    that are available in the neato engine. *)
module NeatoAttributes = struct

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
      (** Initial coordinates of the node. *)
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

  type subgraph = {
    sg_name : string;
    sg_attributes : vertex list;
    sg_parent : string option;
  }

  (** {4 Pretty-print of attributes} *)

  let fprint_graph ppf = function
    #CommonAttributes.graph as att -> CommonAttributes.fprint_graph ppf att
    | `Margin (f1, f2) -> fprintf ppf "margin=\"%f,%f\"" f1 f2
    | `Start i -> fprintf ppf "start=%i" i
    | `Overlap b -> fprintf ppf "overlap=%b" b
    | `Spline b -> fprintf ppf "spline=%b" b
    | `Sep f -> fprintf ppf "sep=%f" f

  let fprint_vertex ppf = function
    #CommonAttributes.vertex as att ->
    CommonAttributes.fprint_vertex ppf att
    | `Pos (f1, f2) -> fprintf ppf "pos=\"%f,%f\"" f1 f2

  let fprint_edge ppf = function
    #CommonAttributes.edge as att -> CommonAttributes.fprint_edge ppf att
    | `Id s -> fprintf ppf "id=%a" fprint_string s
    | `Len f -> fprintf ppf "len=%f" f
    | `Weight f -> fprintf ppf "weight=%f" f

  let fprint_vertex_list =
    CommonAttributes.fprint_attributes
      CommonAttributes.fprint_style_list fprint_vertex

  let fprint_edge_list =
    CommonAttributes.fprint_attributes
      CommonAttributes.fprint_style_list fprint_edge

end

module Neato =
  MakeEngine (struct
    module Attributes = NeatoAttributes
    let name = "neato"
    let opening = "graph"
    let edge_arrow = "--"
  end)

(*
Local Variables:
compile-command: "make -C .."
End:
*)
