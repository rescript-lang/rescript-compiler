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

open XDot
open XDotDraw

let red_color =   0xFF0000FFl
let blue_color =  0x0000FFFFl
let green_color = 0x00FF00FFl
let black_color = 0x000000FFl
let white_color = 0xFFFFFFFFl


exception Cannot_convert_color of string
let rgba_to_int32 col =
  (* Graphivz sometimes insert named colors by itself, we need to catch
     those. The list might be incomplete though; if it is, we might need to
     translate http://www.w3.org/TR/SVG/types.html#ColorKeywords *)
  match col with
  | "black" -> black_color
  | "white" -> white_color
  | "red" -> red_color
  | "green" -> green_color
  | "blue" -> blue_color
  | _ ->
    try
      match String.length col with
      | 7 -> Graph.Graphviz.color_to_color_with_transparency
               (int_of_string ("0x" ^ String.sub col 1 6))
      | 9 -> Int32.of_string ("0x" ^ String.sub col 1 8)
      | _ -> failwith ""
    with Failure _ ->
      (* Can fail either in the failed match above, or in int*of_string *)
      raise (Cannot_convert_color (col^" but int32 expected"))

let convert_fill_color col = `FILL_COLOR_RGBA (rgba_to_int32 col)
let convert_outline_color col = `OUTLINE_COLOR_RGBA (rgba_to_int32 col)

let ($) f x = f x


(* Derived text class. *)
class graph_text txt_obj ~size_points ~(props:GnomeCanvas.text_p list) =
  let props = `SIZE_POINTS size_points :: props in
  object (self)

    inherit GnoCanvas.text txt_obj as text
    val original_size = size_points
    val mutable props = props

    method! set p =
      props <- p;
      text#set p

    method highlight ?(color=red_color,green_color) () =
      let primary, secondary = color in
      let color = ref primary in
      let rec hi_props = function
        | [] -> []
        | `SIZE_POINTS p :: l ->
          let p = if p >= 12. then p else max 6. (p *. 1.5) in
          `SIZE_POINTS p :: hi_props l
        | `WEIGHT d :: l ->
          `WEIGHT (max 600 (int_of_float (float d *. 1.5))) :: hi_props l
        | `FILL_COLOR_RGBA c :: l ->
          if c = primary then color := secondary;
          hi_props l
        | p :: l -> p :: hi_props l
      in
      (* as inserted in head, `WEIGHT 600 will not apply if there is already a
         specified weight *)
      text#set (`FILL_COLOR_RGBA !color :: `WEIGHT 600 :: hi_props props)

    method dehighlight () =
      (* as inserted in head, properties will not apply if they are already
         specified *)
      text#set (`WEIGHT 400 :: `FILL_COLOR_RGBA black_color :: props)

    method resize (zoom_factor:float) =
      let rec change = function
        | [] -> []
        | `SIZE_POINTS _ :: l -> 
          `SIZE_POINTS (zoom_factor*.original_size) :: change l
        | `FONT _ :: l -> change l
        | p :: l -> p :: change l
      in
      self#set (change props)

    initializer
      text#set props

  end

(* Constructor copied and adapted from gnoCanvas.ml *)
let graph_text ?x ?y ?text ?font ?anchor ~size_points ?(props=[]) p =
  let add_prop props f x =
    match x with None -> props | Some x -> f x :: props
  in
  let props = add_prop props (fun x -> `ANCHOR x) anchor in
  let props = add_prop props (fun x -> `FONT x) font in
  let props = add_prop props (fun x -> `TEXT x) text in
  let props = add_prop props (fun x -> `Y x) y in
  let props = add_prop props (fun x -> `X x) x in
  let i = GnomeCanvas.Item.new_item p#as_group GnomeCanvas.Types.text in
  new graph_text i ~size_points ~props

(** FROM DOT LAYOUT TO VIEW ITEMS *)

(* Shape properties.
   Used when initializing items *)

let pen_color draw_st = convert_outline_color draw_st.XDotDraw.pen_color
let fill_color draw_st = convert_fill_color draw_st.XDotDraw.fill_color

(* Flattens an array of pair of coordinates into an array of coordinates *)
let flatten_points pts =
  let convert i =
    let x, y = pts.(i / 2) in
    if i mod 2 = 0 then x else y
  in
  Array.init (2 * Array.length pts) convert

(* SHAPE CONSTRUCTORS *)

(* Ellipse, polygon and bpath canvas items do not share the same type
   in lablgtk2
   They are kept in separate type constructors along with their
   initial properties. *)
type shape_t =
  | SEllipse of GnoCanvas.ellipse
  | SPolygon of GnoCanvas.polygon
  | SBSpline of GnoCanvas.bpath

let shape_item = function
  | SEllipse e -> e#as_item
  | SPolygon p -> p#as_item
  | SBSpline b -> b#as_item

(* Shape properties (used by canvas items ellipse, polygon and bpath) *)
type shape_p =
  [ `FILL_COLOR_RGBA of int32
  | `OUTLINE_COLOR_RGBA of int32
  | `WIDTH_UNITS of float
  | `DASH of float * float array ]

(* Property list completion *)
(* In the initial property list of a shape, we need all the properties to hold
   a value, so that we can refer to them when unsetting property changes *)

let complete_props props =
  let rec what_props (fill, width, outline, dash as acc) = function
    | [] -> acc
    | `FILL_COLOR_RGBA _ :: l -> what_props (true, width, outline, dash) l
    | `WIDTH_UNITS _ :: l -> what_props (fill, true, outline, dash) l
    | `OUTLINE_COLOR_RGBA _ :: l -> what_props (fill, width, true, dash) l
    | `DASH _ :: l -> what_props (fill, width, outline, true) l
  in
  let fill, width, outline, dash =
    what_props (false, false, false, false) props
  in
  let props = if fill then props else `FILL_COLOR_RGBA black_color :: props in
  let props = if width then props else `WIDTH_UNITS 1. :: props in
  let props =
    if outline then props else `OUTLINE_COLOR_RGBA black_color :: props
  in
  if dash then props else `DASH (0., [| |]) :: props

(* Shape class (either a rect, an ellipse, a polygon or a path).
   Uses a properties queue to undo changes *)
class shape ~fill shape init_props = object (self)

  inherit GnoCanvas.base_item (shape_item shape)

  method private set_props props = match shape with
    | SPolygon p -> p#set (props :> GnomeCanvas.polygon_p list)
    | SEllipse e -> e#set (props :> GnomeCanvas.re_p list)
    | SBSpline b -> b#set (props :> GnomeCanvas.bpath_p list)

  (* Properties queue *)
  val mutable props = []

  method set p =
    props <- p;
    self#set_props p

  method highlight ?(color=red_color,green_color) () =
    let primary, secondary = color in
    let color = ref primary in
    let rec hi_props = function
      | [] -> []
      | `WIDTH_UNITS u :: l -> `WIDTH_UNITS (max 3. (u *. 3.)) :: hi_props l
      | `FILL_COLOR_RGBA c :: l when fill ->
        if c = primary then color := secondary;
        hi_props l
      | `OUTLINE_COLOR_RGBA c :: l ->
        if c = primary then color := secondary;
        hi_props l
      | p :: l -> p :: hi_props l
    in
    (* as inserted in head, `WIDTH_UNITS 3. will not apply if there is already
       a specified weight *)
    let props = hi_props props in
    let props = if fill then `FILL_COLOR_RGBA !color :: props else props in
    self#set_props (`OUTLINE_COLOR_RGBA !color :: `WIDTH_UNITS 3. :: props)

  method dehighlight () =
    (* as inserted in head, `WIDTH_UNITS 1. will not apply if there is already
       a  specified weight *)
    let props = if fill then `FILL_COLOR_RGBA black_color :: props else props in
    self#set_props (`WIDTH_UNITS 1. :: `OUTLINE_COLOR_RGBA black_color :: props)

  initializer self#set (complete_props init_props)

end

let get_props draw_st =
  let base_props = [ pen_color draw_st; fill_color draw_st ] in
  let fold_sty_attr props = function
    | Dashed -> `DASH (0., [|10.|]) :: props
    | Dotted -> `DASH (0., [|2.; 10.|])  :: props
    | _ -> props
  in
  List.fold_left fold_sty_attr base_props draw_st.style

let ellipse ~fill draw_st group pos w h =
  let((x1,y1), (x2,y2)) = XDot.bounding_box pos w h in
  let props = get_props draw_st in
  let ellip = GnoCanvas.ellipse group ~x1 ~y1 ~x2 ~y2 ~props in
  new shape ~fill (SEllipse ellip) props

let polygon ~fill draw_st group pts =
  let props = get_props draw_st in
  let points = flatten_points pts in
  let poly = GnoCanvas.polygon group ~points ~props in
  new shape ~fill (SPolygon poly) props

let pathdef pts =
  if List.length pts mod 3 = 1 then
    (* Starting point *)
    let pathdef = GnomeCanvas.PathDef.new_path () in
    let (x0,y0) = List.hd pts in
    GnomeCanvas.PathDef.moveto pathdef x0 y0;
    (* Rest of the spline *)
    let rec curveto = function
      | (x1,y1) :: (x2,y2) :: (x3,y3) :: t ->
        GnomeCanvas.PathDef.curveto pathdef x1 y1 x2 y2 x3 y3;
        curveto t
      | _ -> ()
    in
    curveto (List.tl pts);
    pathdef
  else
    (* failwith "Cannot build pathdef" *)
    assert false

let bspline ~fill draw_st group pts =
  let path =
    pathdef (Array.fold_right (fun p acc -> p :: acc) pts [])
  in
  let props = get_props draw_st in
  let bpath = GnoCanvas.bpath group ~bpath:path ~props in
  new shape ~fill (SBSpline bpath) props

let text draw_st group (x,y) _align anchor label =
  let size_points, font = draw_st.XDotDraw.font in
  let props = [ convert_fill_color draw_st.XDotDraw.pen_color ] in
  let anchor =
    if anchor = -. 1. then `WEST else if anchor = 1.0 then `EAST else `CENTER
  in
  let size_points,height = XDotDraw.string_scale_size font size_points label
  in
  (* y-height/4 because the base line of the text is 1/4th from the bottom *)
  graph_text
    group
    ~x ~y:(y-.height/.4.) ~text:label ~props ~anchor
    ~font ~size_points

class type common_view = object
  inherit GnoCanvas.canvas
  method zoom_factor : float
  method adapt_zoom : unit -> unit
end

(* ITEMS *)

class type textshape = object
  method highlight: ?color:int32 * int32 -> unit -> unit
  method dehighlight: unit -> unit
  method hide: unit -> unit
  method show: unit -> unit
  method lower_to_bottom: unit -> unit
  method connect:
    < event : callback:(GnoCanvas.item_event -> bool) -> GtkSignal.id;
      after : GnoCanvas.item_signals;
      destroy : callback:(unit -> unit) -> GtkSignal.id; >
end

(* DGraph item
   Node and edge derive from this class
   Contains shapes and texts
   Can be : highlighted and/or selected

   ~pos : center of the container
   ~ops_list : list of list of operations
   ~hl_vip : highlight properties, set when method highlight is called *)
class [ 'a ] view_item ~fill ~delay ~(view: common_view) ~pos ~ops_list
    ~(item:'a) =
  object (self)

    inherit GnoCanvas.group view#root#as_group

    val mutable hilighted = false
    val mutable texts = []
    val mutable shapes = []
    val mutable computed = not delay
    val mutable cached_events = []

    method item = item

    method private cache : 'a. ('a -> unit) -> 'a -> unit =
      fun f x ->
        if computed then f x
        else cached_events <- (fun () -> f x) :: cached_events

    method zoom_text (zf:float) =
      self#cache (fun zf -> List.iter (fun t -> t#resize zf) texts) zf

    method private iter f =
      List.iter (fun t -> f (t :> textshape)) texts;
      List.iter f (shapes :> textshape list)

    method highlight ?color () =
      self#cache
        (fun () ->
           if not hilighted then begin
             hilighted <- true;
             self#iter (fun s -> s#highlight ?color ());
             List.iter (fun t -> t#raise_to_top ()) texts;
           end)
        ()

    method dehighlight () =
      self#cache
        (fun () ->
           if hilighted then begin
             hilighted <- false;
             self#iter (fun s -> s#dehighlight ());
           end)
        ()

    method! hide () = self#cache (fun () -> self#iter (fun s -> s#hide ())) ()
    method! show () = self#cache (fun () -> self#iter (fun s -> s#show ())) ()

    method connect_event ~callback =
      self#cache
        (fun () -> self#iter (fun s -> ignore (s#connect#event ~callback)))
        ()

    method center () =
      self#cache(fun () -> 
          let x, y = pos in
          let w = view#hadjustment#page_size /. view#zoom_factor in
          let h = view#vadjustment#page_size /. view#zoom_factor in
          let sx = x -. (w /. 2.) in
          let sy = y -. (h /. 2.) in
          let sx, sy = view#w2c ~wx:sx ~wy:sy in
          ignore $ view#scroll_to ~x:sx ~y:sy) 
        ()

    method! lower_to_bottom () =
      self#cache (fun () -> self#iter (fun s -> s#lower_to_bottom ())) ()

    (* Reads a list of list of operations
       Updates the shapes and texts lists *)
    method private read_operations () =
      let read_op draw_st = function
        (* Create shapes *)
        | XDotDraw.Filled_ellipse (pos, w, h)
        | XDotDraw.Unfilled_ellipse (pos, w, h) ->
          shapes <- ellipse ~fill draw_st self pos w h :: shapes
        | XDotDraw.Filled_polygon pts | XDotDraw.Unfilled_polygon pts ->
          shapes <- polygon ~fill draw_st self pts :: shapes
        | XDotDraw.Bspline pts | XDotDraw.Filled_bspline pts ->
          shapes <- bspline ~fill draw_st self pts :: shapes
        | XDotDraw.Text (pos, align, anchor, label) ->
          texts <- text draw_st self pos align anchor label :: texts
        | _ -> ()
      in
      List.iter (draw_with read_op) ops_list;
      List.iter (fun t -> t#raise_to_top ()) texts;
      List.fold_right (* preserve order *) (fun f () -> f ()) cached_events ()

    method compute () =
      if not computed then begin
        computed <- true;
        self#read_operations ()
      end

    initializer
      if delay then
        let prio = Glib.int_of_priority `LOW in
        ignore (Glib.Idle.add ~prio (fun () -> self#compute (); false))
      else
        self#read_operations ()

  end

let view_node ~delay ~view ~vertex ~layout () =
  let pos = layout.n_pos in
  let ops_list = [ layout.n_ldraw; layout.n_draw ] in
  new view_item ~fill:false ~delay ~view ~pos ~ops_list ~item:vertex

let view_edge ~delay ~view ~edge ~layout () =
  let pos = 0., 0. in
  let ops_list =
    [ layout.e_draw; layout.e_ldraw;
      layout.e_tldraw; layout.e_hldraw;
      layout.e_tdraw; layout.e_hdraw    ]
  in
  new view_item ~fill:true ~delay ~view ~pos ~ops_list ~item:edge

let view_cluster ~delay ~view ~cluster ~layout () =
  let pos = layout.c_pos in
  let ops_list = [ layout.c_ldraw; layout.c_draw ] in
  let view =
    new view_item ~fill:false ~delay ~pos ~ops_list ~view ~item:cluster
  in
  view#lower_to_bottom ();
  view
