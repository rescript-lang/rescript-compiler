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

open Graph

let set_if_none field value = match field with
  | None -> Some value
  | Some a -> Some a

let the = function None -> assert false | Some a -> a

type cluster = string

module Build
    (G: Graphviz.GraphWithDotAttrs)
    (TreeManipulation: sig val is_ghost_node: G.V.t -> bool end) =
struct

  module Layout = struct
    include XDot.Make(G)
    open XDot
    type t = graph_layout =
      { vertex_layouts  : node_layout HV.t;
        edge_layouts    : edge_layout HE.t;
        cluster_layouts : (string, cluster_layout) Hashtbl.t;
        bbox : bounding_box }
  end
  open Layout

  type geometry_info = {
    dimensions : (float * float) HV.t;
    position : (float * float) HV.t;
    mutable x_offset : float;
    mutable y_offset : int;
  }

  let get_position v geometry_info =
    try HV.find geometry_info.position v
    with Not_found -> assert false

  let get_dimensions v geometry_info =
    try HV.find geometry_info.dimensions v
    with Not_found -> assert false

  let set_offset geometry_info =
    geometry_info.y_offset <- 150;
    geometry_info.x_offset <-
      HV.fold (fun _ (w, _) maxw -> max w maxw)
        geometry_info.dimensions 0.

  (* Calculate node positions for a tree *)
  let fill_tree_positions tree root iter_fun fold_fun table geometry_info =
    let vertex_x_space = 10. in
    let stack = Stack.create () in
    let fill_stack tree root =
      let stack_queue = Queue.create () in
      let rec flush_queue queue =
        if not(Queue.is_empty queue) then begin
          let elem, depth = Queue.take queue in
          iter_fun (fun v -> Queue.add (v, depth + 1) queue) tree elem;
          Stack.push (elem,depth) stack;
          flush_queue queue
        end
      in
      Queue.add (root, 0) stack_queue;
      flush_queue stack_queue;
    in
    fill_stack tree root;
    let offset = ref geometry_info.x_offset in
    let max_depth = snd (Stack.top stack) in
    let rec flush_stack stack =
      if not (Stack.is_empty stack) then begin
        let elem, depth = Stack.pop stack in
        if depth = max_depth then begin
          HV.add table elem (!offset, depth);
          offset := !offset +. geometry_info.x_offset +. vertex_x_space;
        end else begin
          let sum, cpt =
            fold_fun
              (fun v (sum, cpt) ->
                 let (x, _) =
                   try HV.find table v with Not_found -> assert false
                 in
                 sum +. x, cpt +. 1.)
              tree
              elem
              (0., 0.)
          in
          assert (cpt <> 0.);
          HV.add table elem (sum /. cpt, depth)
        end;
        flush_stack stack
      end
    in
    flush_stack stack

  (* Bind two tree position tables together *)
  let bind_tree_tables forward_table backward_table root geometry_info =
    (* Using dimension is required in order to be put at the middle of the
       canvas.*)
    let max_fwd, max_dim_fwd =
      HV.fold
        (fun v (_, y) (max_y, max_dimy as acc) ->
           if TreeManipulation.is_ghost_node v then acc
           else (*max y max_y*)
           if y < max_y then acc
           else
             let _, dimy = get_dimensions v geometry_info in
             (*[BM] why 1.5? *)
             let dimy = dimy *. 1.5 in
             y, if y = max_y then max max_dimy dimy else dimy)
        forward_table
        (0, 0.)
    in
    HV.iter
      (fun v (x, y) ->(*[BM]: why 0.5 and 1.5 ??? *)
         HV.add
           geometry_info.position
           v
           (x, ((float (max_fwd + y)) -. 0.5) *. float geometry_info.y_offset
               -. 1.5 *. max_dim_fwd))
      backward_table;
    HV.iter
      (fun v (x, y) ->(*[BM]: why 0.5 and 1.5 ??? *)
         HV.add
           geometry_info.position
           v
           (x, ((float (max_fwd - y)) -. 0.5) *. float geometry_info.y_offset
               -. 1.5 *. max_dim_fwd))
      forward_table;
    HV.remove geometry_info.position root

  (* DRAW OPERATIONS *)

  (* Convert an int in hexadecimal representing a color in rgb format to a
     string prefixed by # *)
  let string_color i = Printf.sprintf "#%06X" i;;
  let string_color32 i = Printf.sprintf "#%08lX" i;;

  (** @return an array of positions to draw an edge from positions and
      dimensions of vertices *)
  let edge_to_posarray src dst geometry_info =
    let xsrc, ysrc = get_position src geometry_info in
    let _, hsrc = get_dimensions src geometry_info in
    let xdst, ydst = get_position dst geometry_info in
    let _, hdst = get_dimensions dst geometry_info in
    let ystart = ysrc -. hsrc/.2. and yend = ydst +. hdst/. 2. in
    (* Bezier intermediate points. *)
    let xdec = (xdst -. xsrc)/.4.0 in
    let ydec = (ydst -. ysrc)/.4.0 in
    (*    Format.printf "%f %f %f@." ystart yend ydec;*)
    [| xsrc, ystart;
       xsrc +. xdec, ystart +. ydec;
       xdst -. xdec, yend -. ydec;
       xdst, yend |];;

  (** @return an array to draw an arrow from start and end positions of the
      edge *)
  let edge_to_arrow (x1, y1) (x2, y2) =
    let warrow = 4. in (* Half-width of the arrow *)
    let harrow = 10. in (* Height of the arrow *)
    let dx = x2 -. x1 in
    let dy = y1 -. y2 in
    let d = sqrt (dx *. dx +. dy *. dy) in
    let xp1 = -. (harrow *. dx +. warrow *. dy) /. d +. x2 in
    let yp1 = (harrow *. dy -. warrow *. dx) /. d +. y2 in
    let xp2 = (warrow *. dy -. harrow *. dx) /. d +. x2 in
    let yp2 = (warrow *. dx +. harrow *. dy) /. d +. y2 in
    [ XDotDraw.Filled_polygon [| x2, y2; xp1, yp1; xp2, yp2 |] ]

end

(* FROM GRAPH *)

module Make
    (Tree: Graphviz.GraphWithDotAttrs)
    (TreeManipulation: sig val is_ghost_node: Tree.V.t -> bool end) =
struct

  include Build(Tree)(TreeManipulation)
  open Layout

  (* PARSE VERTICES ATTRIBUTES *)

  type vattributes = {
    (* See graphviz.mli for the meaning of each options *)
    mutable color : int32 option;
    mutable fontcolor : int option;
    mutable fontname : string option;
    mutable fontsize : int option;
    mutable height : float option;
    mutable label : string option;
    mutable html_label : string option;
    mutable orientation : float option;
    mutable peripheries : int option;
    mutable regular : bool option;
    mutable shape : [ `Ellipse | `Box | `Circle | `Doublecircle | `Diamond
                    | `Oval | `Egg | `Triangle | `Invtriangle
                    | `Trapezium | `Invtrapezium
                    | `House | `Invhouse
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
                    | `Plaintext | `Record | `Polygon of int * float ] option;
    mutable style : [ `Rounded | `Filled | `Solid | `Dashed | `Dotted | `Bold
                    | `Invis ] list;
    mutable width : float option;
    mutable fillcolor : int32 option;
  }

  let set_vattribute vattrs : Graphviz.DotAttributes.vertex  -> _ = function
    | `Color c ->
      vattrs.color <-
        set_if_none vattrs.color (Graphviz.color_to_color_with_transparency c)
    | `ColorWithTransparency c -> vattrs.color <- set_if_none vattrs.color c
    | `Fontcolor c -> vattrs.fontcolor <- set_if_none vattrs.fontcolor c
    | `Fontname n -> vattrs.fontname <- set_if_none vattrs.fontname n
    | `Fontsize s -> vattrs.fontsize <- set_if_none vattrs.fontsize s
    | `Height h -> vattrs.height <- set_if_none vattrs.height h
    | `Label label -> vattrs.label <- set_if_none vattrs.label label
    | `HtmlLabel l -> vattrs.html_label <- set_if_none vattrs.html_label l
    | `Orientation o -> vattrs.orientation <- set_if_none vattrs.orientation o
    | `Peripheries p -> vattrs.peripheries <- set_if_none vattrs.peripheries p
    | `Regular r -> vattrs.regular <- set_if_none vattrs.regular r
    | `Shape shape -> vattrs.shape <- set_if_none vattrs.shape shape
    | `Style s -> vattrs.style <- s :: vattrs.style
    | `Width w -> vattrs.width <- set_if_none vattrs.width w
    | `Fillcolor c ->
      vattrs.fillcolor <- set_if_none vattrs.fillcolor
          (Graphviz.color_to_color_with_transparency c)
    | `FillcolorWithTransparency c ->
      vattrs.fillcolor <- set_if_none vattrs.fillcolor c
    | `Comment _ | `Distortion _ | `Fixedsize _ | `Layer _ | `Penwidth _
    | `Url _ | `Z _ ->
      () (* TODO *)

  let attributes_list_to_vattributes vattrs =
    List.iter (set_vattribute vattrs)

  let fill_vattributes tree vattributes =
    let vertex_to_vattrs v =
      let vattrs = {
        color = None;
        fontcolor = None;
        fontname = None;
        fontsize = None;
        height = None;
        label = None;
        html_label = None;
        orientation = None;
        peripheries = None;
        regular = None;
        shape = None;
        style = [];
        width = None;
        fillcolor = None
      } in
      let dgraph_layout_default =
        [ `Color 0xFFFFFF; `Fontcolor 0x000000; `Fontname "Sans";
          `Fontsize 12; `Height 0.; `Label (Tree.vertex_name v);
          `Orientation 0.; `Peripheries 1; `Regular false; `Shape `Ellipse;
          `Width 0.; `Fillcolor 0xFFFFFF ]
      in
      attributes_list_to_vattributes vattrs
        (Tree.vertex_attributes v
         @ Tree.default_vertex_attributes tree
         @ dgraph_layout_default);
      vattrs
    in
    Tree.iter_vertex (fun v -> HV.add vattributes v (vertex_to_vattrs v)) tree

  (* PLACE VERTICES *)

  (* Calculate dimension of a string in pixel *)
  let calc_dimensions
      family
      ptsize
      ?(weight=`NORMAL)
      ?(style=`NORMAL)
      s
      context_obj
    =
    let width_margin = 20. in
    let height_margin = 0. in
    let font_description = Pango.Font.from_string "" in
    Pango.Font.modify font_description
      ~family:family
      ~weight
      ~style
      ~size:(ptsize * Pango.scale)
      ();
    let context = GtkBase.Widget.create_pango_context context_obj in
    Pango.Context.set_font_description context font_description;
    let layout = Pango.Layout.create context in
    Pango.Layout.set_text layout s;
    let width, height = Pango.Layout.get_pixel_size layout in
    float width +. width_margin, float height +. height_margin

  let fill_dimensions context tree vattributes geometry_info =
    let add_vertex_dimensions v =
      let vattrs = try HV.find vattributes v with Not_found -> assert false in
      let minwidth, minheight = the vattrs.width, the vattrs.height in
      let truewidth, trueheight =
        calc_dimensions
          (the vattrs.fontname)
          (the vattrs.fontsize)
          (the vattrs.label) context
      in
      let width = max minwidth truewidth in
      let height = max minheight trueheight in
      HV.replace geometry_info.dimensions v (width, height)
    in
    Tree.iter_vertex add_vertex_dimensions tree

  let fill_position tree root geometry_info =
    let forward_table = HV.create 97 in
    let backward_table = HV.create 97 in
    fill_tree_positions tree root Tree.iter_succ Tree.fold_succ forward_table
      geometry_info;
    fill_tree_positions tree root Tree.iter_pred Tree.fold_pred backward_table
      geometry_info;
    bind_tree_tables forward_table backward_table root geometry_info

  (* BUILD LAYOUT - ADD DRAW OPERATIONS *)

  let style_to_style_attr = function
    | `Filled -> XDotDraw.Filled
    | `Solid -> XDotDraw.Solid
    | `Dashed -> XDotDraw.Dashed
    | `Dotted -> XDotDraw.Dotted
    | `Bold -> XDotDraw.Bold
    | `Invis -> XDotDraw.Invisible
    | `Rounded -> XDotDraw.Rounded;;

  (* FOR VERTEX *)

  let shape_to_operations v vattrs geometry_info shape =
    let width, height =
      let a, b = get_dimensions v geometry_info in
      a /. 2., b
    in
    let position = get_position v geometry_info in
    let filled = List.mem `Filled vattrs.style in
    match shape with
    | `Ellipse ->
      if filled then [ XDotDraw.Filled_ellipse (position,width,height) ]
      else [ XDotDraw.Unfilled_ellipse (position,width,height) ]
    | `Circle ->
      let diameter = max width height in
      if filled then [ XDotDraw.Filled_ellipse (position,diameter,diameter) ]
      else [ XDotDraw.Unfilled_ellipse (position,diameter,diameter) ]
    | `Doublecircle ->
      let diameter = max width height in
      let big_diameter = diameter +. 5. in
      (XDotDraw.Unfilled_ellipse (position,big_diameter,big_diameter)) ::
      [ if filled then XDotDraw.Filled_ellipse (position,diameter,diameter)
        else XDotDraw.Unfilled_ellipse (position,diameter,diameter) ]
    | `Box ->
      let x, y = position in
      let x1 = x -. width and x2 = x +. width in
      let y1 = y -. height and y2 = y +. height in
      let pos_array = [|(x1,y1);(x1,y2);(x2,y2);(x2,y1)|] in
      if filled then [ XDotDraw.Filled_polygon pos_array ]
      else [ XDotDraw.Unfilled_polygon pos_array ]
    | `Record ->
      let x, y = position in
      let x1 = x -. width and x2 = x +. width in
      let y1 = y -. height and y2 = y +. height in
      let pos_array = [|(x1,y1);(x1,y2);(x2,y2);(x2,y1)|] in
      if filled then [ XDotDraw.Filled_polygon pos_array ]
      else [ XDotDraw.Unfilled_polygon pos_array ]
    | `Diamond ->
      let x, y = position in
      let x1 = x -. width and x2 = x +. width in
      let y1 = y -. height and y2 = y +. height in
      let pos_array = [|(x,y1);(x1,y);(x,y2);(x2,y)|] in
      if filled then [ XDotDraw.Filled_polygon pos_array ]
      else [ XDotDraw.Unfilled_polygon pos_array ]
    |_ -> [ XDotDraw.Unfilled_ellipse ((0.,0.),0.,0.) ];;

  let vattrs_to_draw_operations v vattributes geometry_info =
    let vattrs = try HV.find vattributes v with Not_found -> assert false in
    let width, _height = get_dimensions v geometry_info in
    (* Vertex shape drawing *)
    XDotDraw.Pen_color (string_color32 (the vattrs.color)) ::
    XDotDraw.Style (List.map (style_to_style_attr) vattrs.style) ::
    (if List.mem `Filled vattrs.style then
       XDotDraw.Fill_color (string_color32 (the vattrs.fillcolor)) ::
       shape_to_operations v vattrs geometry_info (the vattrs.shape)
     else
       shape_to_operations v vattrs geometry_info (the vattrs.shape))
    ,
    (* Vertex label drawing *)
    [ XDotDraw.Pen_color (string_color (the vattrs.fontcolor));
      XDotDraw.Font
        (float (the vattrs.fontsize),
         the vattrs.fontname);
      let x, y = get_position v geometry_info in
      let _, h = get_dimensions v geometry_info in
      (* [JS 2010/10/08] "/. 4." is quite strange but gives better results *)
      XDotDraw.Text
        ((x, y +. h /. 4.),
         XDotDraw.Center,
         width,
         the vattrs.label) ]

  let vertex_to_node_layout v vattributes geometry_info =
    let draw, ldraw = vattrs_to_draw_operations v vattributes geometry_info in
    let width, height = get_dimensions v geometry_info in
    let abs, ord = get_position v geometry_info in
    {
      XDot.n_name = Tree.vertex_name v;
      XDot.n_pos = (abs,ord);
      XDot.n_bbox = XDot.bounding_box (abs, ord) width height;
      XDot.n_draw = draw;
      XDot.n_ldraw = ldraw
    }

  (* FOR CLUSTER *)

  open Graphviz.DotAttributes

  let get_clusters tree =
    let clusters = Hashtbl.create 20 in
    Tree.iter_vertex
      (fun v -> match Tree.get_subgraph v with
         | None -> ()
         | Some c -> Hashtbl.add clusters c v)
      tree;
    clusters;;

  let rec get_cluster_color = function
    | [] -> 0x000000
    | `Color c :: _ -> c
    | _ :: q -> get_cluster_color q;;

  let find_cluster_corners l geometry_info =
    let max_x_distance = 2. *. geometry_info.x_offset in
    let max_y_distance = 2. *. float geometry_info.y_offset in
    let rec find_corners l corners_array =
      let (minx,miny) = corners_array.(0) in
      let (maxx,maxy) = corners_array.(3) in
      match l with
      |[] -> corners_array
      |v :: tl ->
        let x, y = get_position v geometry_info in
        let w, h = get_dimensions v geometry_info in
        let halfw = w /. 2. in
        let x1 = x -. halfw and x2 = x +. halfw in
        let y1 = y -. h and y2 = y +. h in
        (* Should cluster be split in two *)
        let x1_distance = minx -. x1 in
        let x2_distance = x2 -. maxx in
        let y1_distance = miny -. y1 in
        let y2_distance = y2 -. maxy in
        if x1_distance > max_x_distance ||
           x2_distance > max_x_distance ||
           y1_distance > max_y_distance ||
           y2_distance > max_y_distance ||
           ((x1_distance <> 0. || x2_distance <> 0.) &&
            (y1_distance <> 0. || y2_distance <> 0.))
        then
          Array.append (find_corners tl corners_array)
            (find_corners tl [| x1, y1; x1, y2; x2, y2; x2, y1 |])
        else
          let newminx = min x1 minx in
          let newminy = min y1 miny in
          let newmaxx = max x2 maxx in
          let newmaxy = max y2 maxy in
          find_corners tl [|(newminx,newminy);(newminx,newmaxy);
                            (newmaxx,newmaxy);(newmaxx,newminy)|]
    in
    match l with
    | [] ->
      let z = 0., 0. in
      Array.make 4 z
    | v :: q ->
      let x, y = get_position v geometry_info in
      let w, h = get_dimensions v geometry_info in
      let halfw = w /. 2. in
      let x1 = x -. halfw in
      let x2 = x +. halfw in
      let y1 = y -. h in
      let y2 = y +. h in
      find_corners q [| x1, y1; x1, y2; x2, y2; x2, y1 |];;

  let cluster_to_cluster_layout _tree c clusters geometry_info =
    let border_padding = 10. in
    let vertices =
      try Hashtbl.find_all clusters c
      with Not_found -> assert false
    in
    let corners_array = find_cluster_corners vertices geometry_info in
    let add_padding corners_array =
      let (x1,y1) = corners_array.(0) in
      let (x2,y2) = corners_array.(3) in
      let x1_padded = x1 -. border_padding in
      let x2_padded = x2 +. border_padding in
      let y1_padded = y1 -. border_padding in
      let y2_padded = y2 +. border_padding in
      [|(x1_padded,y1_padded);(x1_padded,y2_padded);
        (x2_padded,y2_padded);(x2_padded,y1_padded)|]
    in
    let rec _cut_corners_array corners_array =
      ignore (assert false);
      (* [JS 2010/09/09] does not work:
         exponential time seems to be required! *)
      let length = Array.length corners_array in
      if length > 4 then
        XDotDraw.Unfilled_polygon (add_padding (Array.sub corners_array 0 4)) ::
        (_cut_corners_array (Array.sub corners_array 4 (length-4)))
      else
        [ XDotDraw.Unfilled_polygon (add_padding corners_array) ]
    in
    let (x1,y1) = corners_array.(0) in
    let (x2,y2) = corners_array.(3) in
    {
      XDot.c_pos = ((x1 +. x2) /. 2., (y1 +. y2) /. 2.);
      XDot.c_bbox = ((x1,y1),(x2,y2));
      XDot.c_draw =
        XDotDraw.Pen_color
          (string_color (get_cluster_color c.sg_attributes)) ::
        (*cut_corners_array corners_array*)[];
      XDot.c_ldraw = []
    };;

  (* FOR EDGE *)

  type eattributes = {
    (* See graphviz.mli for the meaning of each field *)
    mutable color : int32 option;
    mutable decorate : bool option;
    mutable dir : [ `Forward | `Back | `Both | `None ] option;
    mutable fontcolor : int option;
    mutable fontname : string option;
    mutable fontsize : int option;
    mutable label : string option;
    mutable html_label : string option;
    mutable labelfontcolor : int option;
    mutable labelfontname : string option;
    mutable labelfontsize : int option;
    mutable style : [ `Solid | `Dashed | `Dotted | `Bold | `Invis ] list
  }

  let rec attributes_list_to_eattributes eattrs : edge list -> _ = function
    |[] -> ()
    | `Color c :: q ->
      eattrs.color <-
        set_if_none eattrs.color (Graphviz.color_to_color_with_transparency c);
      attributes_list_to_eattributes eattrs q
    | `ColorWithTransparency c :: q ->
      eattrs.color <- set_if_none eattrs.color c;
      attributes_list_to_eattributes eattrs q
    | `Decorate d :: q ->
      eattrs.decorate <- set_if_none eattrs.decorate d;
      attributes_list_to_eattributes eattrs q
    | `Dir d :: q ->
      eattrs.dir <- set_if_none eattrs.dir d;
      attributes_list_to_eattributes eattrs q
    | `Fontcolor c :: q ->
      eattrs.fontcolor <- set_if_none eattrs.fontcolor c;
      attributes_list_to_eattributes eattrs q
    | `Fontname n :: q ->
      eattrs.fontname <- set_if_none eattrs.fontname n;
      attributes_list_to_eattributes eattrs q
    | `Fontsize s :: q ->
      eattrs.fontsize <- set_if_none eattrs.fontsize s;
      attributes_list_to_eattributes eattrs q
    | `Label l :: q ->
      eattrs.label <- set_if_none eattrs.label l;
      attributes_list_to_eattributes eattrs q
    | `HtmlLabel l :: q ->
      eattrs.html_label <- set_if_none eattrs.html_label l;
      attributes_list_to_eattributes eattrs q
    | `Labelfontcolor c :: q ->
      eattrs.fontcolor <- set_if_none eattrs.fontcolor c;
      attributes_list_to_eattributes eattrs q
    | `Labelfontname n :: q ->
      eattrs.labelfontname <- set_if_none eattrs.labelfontname n;
      attributes_list_to_eattributes eattrs q
    | `Labelfontsize s :: q ->
      eattrs.labelfontsize <- set_if_none eattrs.labelfontsize s;
      attributes_list_to_eattributes eattrs q
    | `Style s :: q ->
      eattrs.style <- s :: eattrs.style;
      attributes_list_to_eattributes eattrs q
    | (`Arrowhead _ | `Arrowsize _ | `Arrowtail _ | `Comment _  | `Constraint _
      | `Headlabel _ | `Headport _ | `Headurl _ | `Labelangle _
      |`Labeldistance _ | `Labelfloat _ | `Layer _ | `Minlen _ | `Penwidth _
      | `Samehead _  | `Sametail _ | `Taillabel _ | `Tailport _ | `Tailurl _
      | `Weight _ ) :: q ->
      attributes_list_to_eattributes eattrs q;;

  let eattrs_to_operation tree e geometry_info =
    let eattrs = {
      color = None;
      decorate = None;
      dir = None;
      fontcolor = None;
      fontname = None;
      fontsize = None;
      label = None;
      html_label = None;
      labelfontcolor = None;
      labelfontname = None;
      labelfontsize = None;
      style = [] }
    in
    let dgraph_layout_default =
      [ `Color 0xFF0000; `Decorate false; `Dir `Forward; `Fontcolor 0x00000;
        `Fontname "Sans"; `Fontsize 12; `Label ""; `Labelfontcolor 0x000000;
        `Labelfontname "Sans"; `Labelfontsize 12; `Style `Solid ]
    in
    attributes_list_to_eattributes eattrs
      (Tree.default_edge_attributes tree
       @ Tree.edge_attributes e
       @ dgraph_layout_default);
    let posarray =
      edge_to_posarray (Tree.E.src e) (Tree.E.dst e) geometry_info
    in
    let xsrc, ysrc = posarray.(0) in
    let xend, yend = posarray.(3) in
    (
      (* Shapes and curves *)
      [ XDotDraw.Pen_color (string_color32 (the eattrs.color));
        XDotDraw.Fill_color (string_color32 (the eattrs.color));
        XDotDraw.Style (List.map (style_to_style_attr) eattrs.style);
        XDotDraw.Filled_bspline posarray ]
      ,
      (* Label drawing *)
      [ XDotDraw.Pen_color (string_color (the eattrs.fontcolor));
        XDotDraw.Fill_color (string_color (the eattrs.fontcolor));
        XDotDraw.Font (float_of_int (the eattrs.fontsize),
                       (the eattrs.fontname));
        (let pos = ((xsrc +. xend) /. 2. +. 5., (ysrc +. yend) /. 2.) in
         XDotDraw.Text (pos,XDotDraw.Center,40.,the eattrs.label)) ]
      ,
      (* Head arrowhead drawing *)
      (if eattrs.dir = Some `None then
         []
       else
         XDotDraw.Pen_color (string_color32 (the eattrs.color)) ::
         XDotDraw.Fill_color (string_color32 (the eattrs.color)) ::
         XDotDraw.Style (List.map (style_to_style_attr) eattrs.style) ::
         (edge_to_arrow posarray.(2) posarray.(3)))
      ,
      (* Tail arrowhead drawing *)
      []
      ,
      (* Head label drawing *)
      []
      ,
      (* Tail label drawing *)
      []
    )

  let edge_to_edge_layout tree e geometry_info =
    let (draw,ldraw,hdraw,tdraw,hldraw,tldraw) =
      eattrs_to_operation tree e geometry_info
    in
    {
      XDot.e_draw = draw;
      XDot.e_ldraw = ldraw;
      XDot.e_hdraw = hdraw;
      XDot.e_tdraw = tdraw;
      XDot.e_hldraw = hldraw;
      XDot.e_tldraw = tldraw
    }

  (* Graph *)
  let from_tree context tree root =
    let vattributes = HV.create 97 in
    fill_vattributes tree vattributes;
    let geometry_info =
      { dimensions = HV.create 97;
        position = HV.create 97;
        x_offset = 0.;
        y_offset = 0 }
    in
    fill_dimensions context tree vattributes geometry_info;
    set_offset geometry_info;
    fill_position tree root geometry_info;

    let vertex_layouts = HV.create 97 in
    Tree.iter_vertex
      (fun v ->
         let n_layout = vertex_to_node_layout v vattributes geometry_info in
         HV.add vertex_layouts v n_layout)
      tree;

    let edge_layouts = HE.create 97 in
    Tree.iter_edges_e
      (fun e ->
         let e_layout = edge_to_edge_layout tree e geometry_info in
         HE.add edge_layouts e e_layout)
      tree;

    let cluster_layouts = Hashtbl.create 7
    (* [JS 2010/09/09] does not work *)
    (*  build_cluster_layouts tree geometry_info*)
    in
    { vertex_layouts = vertex_layouts;
      edge_layouts = edge_layouts;
      cluster_layouts = cluster_layouts;
      bbox =
        let ((_,_), (_,_) as bb) =
          HV.fold
            (fun v (x, y) ((minx, miny),(maxx, maxy) as acc) ->
               if TreeManipulation.is_ghost_node v then acc
               else (min x minx, min y miny), (max x maxx, max y maxy))
            geometry_info.position
            ((max_float, max_float), (0., 0.))
        in
        (*  Format.printf "BB=%f %f %f %f@." x1 y1 x2 y2;*)
        bb }

end

module MakeFromDotModel
    (Tree: Sig.G with type V.label = DGraphModel.DotG.V.t
                  and type E.label = unit)
    (TreeManipulation: sig val is_ghost_node: Tree.V.t -> bool end) =
struct

  module Tree = struct
    include Tree
    let graph_attributes _ = []
    let default_vertex_attributes _ = []
    let default_edge_attributes _ = []
    let vertex_name _ = ""
    let vertex_attributes _ = []
    let edge_attributes _ = []
    let get_subgraph _ = None
  end

  include Build(Tree)(TreeManipulation)
  open Layout

  (* POSITIONS *)
  let fill_dimensions model tree geometry_info =
    let corners pos_array =
      let p0 = pos_array.(0) in
      Array.fold_left
        (fun ((minx, miny), (maxx, maxy)) (x, y) ->
           (min minx x, min miny y), (max maxx x, max maxy y))
        (p0, p0)
        pos_array
    in
    (* The width and height of the bounding box of the first shape
       corresponding to a node.*)
    let rec get_size = function
      | [] -> 0., 0.
      | XDotDraw.Unfilled_ellipse (_,w,h) :: _
      | XDotDraw.Filled_ellipse (_,w,h) :: _ ->
        2. *. w, 2. *. h
      | XDotDraw.Unfilled_polygon pos_array :: _
      | XDotDraw.Filled_polygon pos_array :: _ ->
        let (minx, miny), (maxx, maxy) = corners pos_array in
        maxx -. minx, maxy -. miny
      | (XDotDraw.Style _ |XDotDraw.Font _|XDotDraw.Pen_color _
        |XDotDraw.Fill_color _|XDotDraw.Filled_bspline _ |XDotDraw.Bspline _
        | XDotDraw.Polyline _|XDotDraw.Text _)
        :: tl -> get_size tl
    in
    Tree.iter_vertex
      (fun v ->
         let layout = model#get_vertex_layout (Tree.V.label v) in
         let dim = get_size layout.XDot.n_draw in
         HV.add geometry_info.dimensions v dim)
      tree

  let fill_position tree root geometry_info =
    let forward_table = HV.create 97 in
    let backward_table = HV.create 97 in
    fill_tree_positions tree root Tree.iter_succ Tree.fold_succ forward_table
      geometry_info;
    (*HV.iter (fun k (off,depth) -> Format.printf "off:%f depth:%d@." off depth)
      forward_table;*)
    fill_tree_positions tree root Tree.iter_pred Tree.fold_pred backward_table
      geometry_info;
    (*    HV.iter (fun k (off,depth) ->
          Format.printf "BACKoff:%f depth:%d@." off depth)
          backward_table;
          Format.printf "DONE@.";*)
    bind_tree_tables forward_table backward_table root geometry_info

  (* VERTICES *)
  let parse_n_draw_operations operations (abs, ord as pos) =
    let polygon pts =
      let length = float (Array.length pts) in
      let oldabssum, oldordsum =
        Array.fold_left
          (fun (xsum, ysum) (x, y) -> xsum +. x, ysum +. y) (0.,0.) pts
      in
      let oldabs, oldord = oldabssum /. length, oldordsum /. length in
      Array.map (fun (x, y) -> x -. oldabs +. abs, y -. oldord +. ord) pts
    in
    let do_one = function
      | XDotDraw.Unfilled_ellipse (_, w, h) ->
        XDotDraw.Unfilled_ellipse (pos, w, h)
      | XDotDraw.Filled_ellipse (_, w, h) -> XDotDraw.Filled_ellipse (pos, w, h)
      | XDotDraw.Filled_polygon pts -> XDotDraw.Filled_polygon (polygon pts)
      | XDotDraw.Unfilled_polygon pts -> XDotDraw.Unfilled_polygon (polygon pts)
      | op -> op
    in
    List.map do_one operations

  let parse_n_ldraw_operations
      (initial_node_pos_x,initial_node_pos_y)
      (node_pos_x,node_pos_y)
      operations
    =
    List.map
      (function
        | XDotDraw.Text ((pos_x,pos_y), align, w, s) ->
          let translate_x,translate_y =
            node_pos_x-.initial_node_pos_x,node_pos_y-.initial_node_pos_y
          in
          let (_,_ as pos) = (* same affine move as the attached node has had*)
            pos_x+.translate_x,
            pos_y+.translate_y
          in
          XDotDraw.Text (pos, align, w, s)
        | op -> op)
      operations

  let parse_vertex_layout _tree v orig_layout geometry_info =
    let width, height = get_dimensions v geometry_info in
    let (_,_ as pos) = get_position v geometry_info in
    { XDot.n_name = orig_layout.XDot.n_name;
      n_pos = pos;
      n_bbox = XDot.bounding_box pos width height;
      n_draw = parse_n_draw_operations orig_layout.XDot.n_draw pos;
      n_ldraw = parse_n_ldraw_operations
          orig_layout.XDot.n_pos
          pos
          orig_layout.XDot.n_ldraw}

  (* EDGES *)
  let rec parse_e_draw_operations operations src dst geometry_info =
    match operations with
    | [] -> []
    | XDotDraw.Bspline _ :: tl ->
      let pos_array = edge_to_posarray src dst geometry_info in
      XDotDraw.Bspline pos_array ::
      (edge_to_arrow pos_array.(2) pos_array.(3)) @
      (parse_e_draw_operations tl src dst geometry_info)
    | XDotDraw.Filled_bspline _ :: tl ->
      let pos_array = edge_to_posarray src dst geometry_info in
      XDotDraw.Filled_bspline pos_array ::
      (edge_to_arrow pos_array.(2) pos_array.(3)) @
      (parse_e_draw_operations tl src dst geometry_info)
    | XDotDraw.Pen_color c :: tl ->
      XDotDraw.Pen_color c :: XDotDraw.Fill_color c ::
      (parse_e_draw_operations tl src dst geometry_info)
    | op :: tl -> op :: (parse_e_draw_operations tl src dst geometry_info);;

  let rec parse_e_ldraw_operations operations src dst geometry_info =
    match operations with
    | [] -> []
    | XDotDraw.Text (_, align, w, s) :: tl ->
      let (xsrc,ysrc) = get_position src geometry_info in
      let (xdst,ydst) = get_position dst geometry_info in
      let pos = ((xsrc +. xdst) /. 2., (ysrc +. ydst) /. 2.) in
      XDotDraw.Text (pos, align, w, s) ::
      (parse_e_ldraw_operations tl src dst geometry_info)
    | op :: tl -> op :: (parse_e_ldraw_operations tl src dst geometry_info);;

  let parse_edge_layout _tree e layout geometry_info =
    let src = Tree.E.src e and dst = Tree.E.dst e in
    {
      XDot.e_draw =
        parse_e_draw_operations layout.XDot.e_draw src dst geometry_info;
      e_ldraw =
        parse_e_ldraw_operations layout.XDot.e_ldraw src dst geometry_info;
      e_hdraw = [];
      e_tdraw = [];
      e_hldraw = [];
      e_tldraw = []
    };;

  (* CLUSTERS *)

  let from_model tree root model =
    let geometry_info =
      { dimensions = HV.create 97;
        position = HV.create 97;
        x_offset = 0.;
        y_offset = 0 }
    in
    fill_dimensions model tree geometry_info;
    set_offset geometry_info;
    fill_position tree root geometry_info;
    let vertex_layouts = HV.create 97 in
    Tree.iter_vertex
      (fun v ->
         let old_layout = model#get_vertex_layout (Tree.V.label v) in
         let v_layout = parse_vertex_layout tree v old_layout geometry_info in
         HV.add vertex_layouts v v_layout)
      tree;
    let edge_layouts = HE.create 97 in
    Tree.iter_edges_e
      (fun e ->
         let src = Tree.V.label (Tree.E.src e) in
         let dst = Tree.V.label (Tree.E.dst e) in
         let old_layout =
           try model#get_edge_layout (model#find_edge src dst)
           with Not_found ->
             { XDot.e_draw = [];
               e_ldraw = [];
               e_hdraw = [];
               e_tdraw = [];
               e_hldraw = [];
               e_tldraw = [] }
         in
         let e_layout = parse_edge_layout tree e old_layout geometry_info in
         HE.add edge_layouts e e_layout)
      tree;
    let cluster_layouts = Hashtbl.create 7 in
    let root_pos = get_position root geometry_info in
    { vertex_layouts = vertex_layouts;
      edge_layouts = edge_layouts;
      cluster_layouts = cluster_layouts;
      bbox =
        let ((_,_), (_,_) as bb) =
          HV.fold
            (fun v (x, y) ((minx, miny),(maxx, maxy) as acc) ->
               if TreeManipulation.is_ghost_node v then acc
               else
                 let width,height= get_dimensions v geometry_info in
                 (min (x-.width) minx, min (y-.height) miny),
                 (max (x+.width) maxx, max (y+.height) maxy))
            geometry_info.position
            (root_pos, root_pos)
        in
        (*  Format.printf "BB=%f %f %f %f@." x1 y1 x2 y2;*)
        bb }

end
