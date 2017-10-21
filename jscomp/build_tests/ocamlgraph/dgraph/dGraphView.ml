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

open DGraphViewItem

let ($) f x = f x

class type ['vertex, 'edge, 'cluster] view = object
  inherit GnoCanvas.canvas
  method model : ('vertex, 'edge, 'cluster) DGraphModel.abstract_model
  method get_node : 'vertex -> 'vertex view_item
  method get_edge : 'edge -> 'edge view_item
  method get_cluster : 'cluster -> 'cluster view_item
  method iter_nodes:  ('vertex view_item -> unit) -> unit
  method iter_edges: ('vertex view_item -> 'vertex view_item -> unit) -> unit
  method iter_edges_e:  ('edge view_item -> unit) -> unit
  method iter_clusters: ('cluster view_item -> unit) -> unit
  method iter_succ: ('vertex view_item -> unit) -> 'vertex view_item -> unit
  method iter_pred: ('vertex view_item -> unit) -> 'vertex view_item -> unit
  method iter_succ_e: ('edge view_item -> unit) -> 'vertex view_item -> unit
  method iter_pred_e: ('edge view_item -> unit) -> 'vertex view_item -> unit
  method iter_associated_vertex:
    ('vertex view_item -> unit) -> 'vertex view_item -> unit
  method mem_edge: 'vertex view_item -> 'vertex view_item -> bool
  method find_edge: 'vertex view_item -> 'vertex view_item -> 'edge view_item
  method src: 'edge view_item -> 'vertex view_item
  method dst: 'edge view_item -> 'vertex view_item
  method zoom_factor : float
  method zoom_to : float -> unit
  method zoom_in : unit -> unit
  method zoom_out : unit -> unit
  method adapt_zoom : unit -> unit
  method center_node: 'vertex view_item -> unit
  method set_zoom_padding: float -> unit
  method connect_highlighting_event: unit -> unit
  method highlight: ?color: int32 * int32 -> 'vertex view_item -> unit
  method dehighlight: 'vertex view_item -> unit
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

(* ************************************************************************* *)
(** View from a model *)
(* ************************************************************************* *)

module Make(V: Sig.HASHABLE)(E: Sig.HASHABLE)(C: Sig.HASHABLE) = struct

  type vertex = V.t
  type edge = E.t
  type cluster = C.t

  module HV = Hashtbl.Make(V)
  module HE = Hashtbl.Make(E)
  module HC = Hashtbl.Make(C)

  (* Widget derived from Gnome Canvas.
     Supports zooming and scrolling *)
  class view
      ?delay_node ?delay_edge ?delay_cluster
      obj
      (model : (V.t, E.t, C.t) DGraphModel.abstract_model)
    =
    let delay f v = match f with None -> false | Some f -> f v in
    let (x1, y1), (x2, y2) = model#bounding_box in
    object(self)

      inherit GnoCanvas.canvas obj

      method model = model

      (* Hash tables from the model to the view items*)
      val node_hash : V.t view_item HV.t = HV.create 17
      val edge_hash : E.t view_item HE.t = HE.create 17
      val cluster_hash : C.t view_item HC.t = HC.create 7

      (* Canvas items creation *)

      method private add_vertex vertex =
        try
          let layout = model#get_vertex_layout vertex in
          let item =
            view_node
              ~delay:(delay delay_node vertex)
              ~view:(self :> common_view) ~vertex ~layout ()
          in
          HV.add node_hash vertex item
        with Not_found ->
          assert false

      method private add_edge edge =
        try
          let layout = model#get_edge_layout edge in
          let item =
            view_edge
              ~delay:(delay delay_edge edge)
              ~view:(self:>common_view) ~edge ~layout ()
          in
          HE.add edge_hash edge item
        with Not_found ->
          assert false

      method private add_cluster cluster =
        let layout = model#get_cluster_layout cluster in
        let item =
          view_cluster
            ~delay:(delay delay_cluster cluster)
            ~view:(self :> common_view) ~cluster ~layout ()
        in
        HC.add cluster_hash cluster item

      (* From model to view items *)

      method get_node n =
        try HV.find node_hash n with Not_found -> assert false

      method get_edge e =
        try HE.find edge_hash e with Not_found -> assert false

      method get_cluster c =
        try HC.find cluster_hash c with Not_found -> assert false

      (* Iterate on nodes and edges *)
      method iter_nodes f = HV.iter (fun _ v -> f v) node_hash
      method iter_edges_e f = HE.iter (fun _ e -> f e) edge_hash
      method iter_clusters f = HC.iter (fun _ c -> f c) cluster_hash

      method iter_edges f =
        model#iter_edges (fun v1 v2 -> f (self#get_node v1) (self#get_node v2))

      (* Iterate on successors of a node *)
      method iter_succ f (node: 'v view_item) =
        let f' v = f (self#get_node v) in
        model#iter_succ f' node#item

      (* Iterate on predecessors of a node *)
      method iter_pred f (node: 'v view_item) =
        let f' v = f (self#get_node v) in
        model#iter_pred f' node#item

      method iter_succ_e f (node: 'v view_item) =
        let f' e = f (self#get_edge e) in
        model#iter_succ_e f' node#item

      method iter_pred_e f (node: 'v view_item) =
        let f' e = f (self#get_edge e) in
        model#iter_pred_e f' node#item

      (* Iterate on associated nodes *)
      method iter_associated_vertex f (node: 'v view_item) =
        let f' v = f (self#get_node v) in
        model#iter_associated_vertex f' node#item

      (* Membership functions *)

      method mem_edge (n1:'v view_item) (n2:'v view_item) =
        model#mem_edge n1#item n2#item

      method find_edge (n1:'v view_item) (n2:'v view_item) =
        self#get_edge (model#find_edge n1#item n2#item)

      method src (e: 'e view_item) = self#get_node (model#src e#item)
      method dst (e: 'e view_item) = self#get_node (model#dst e#item)

      (* Zoom factor *)
      val mutable zoom_f = 1.
      method zoom_factor = zoom_f

      val mutable zoom_padding = 0.1
      method set_zoom_padding n = zoom_padding <- n

      method private set_zoom_f x = if x > 1e-10 then zoom_f <- x

      (* Zoom to a particular factor *)
      method zoom_to x =
        self#set_zoom_f x;
        self#set_pixels_per_unit zoom_f;
        self#iter_clusters (fun c -> c#zoom_text zoom_f);
        self#iter_nodes (fun n -> n#zoom_text zoom_f);
        self#iter_edges_e (fun e -> e#zoom_text zoom_f)

      method zoom_in () = self#zoom_to (zoom_f +. zoom_padding *. zoom_f)
      method zoom_out () = self#zoom_to (zoom_f -. zoom_padding *. zoom_f)
      method center_node (node:V.t view_item) = 
        node#center ()
      (*      self#zoom_in ();*)


      method adapt_zoom () =
        let width = self#hadjustment#page_size in
        let height = self#vadjustment#page_size in
        let w_zoom = width /. abs_float (x1-.x2) in
        let h_zoom = height /. abs_float (y1-.y2) in
        self#zoom_to (min 1. (min w_zoom h_zoom))

      (* EVENTS *)

      (* Zoom with the keys *)
      method private zoom_keys_ev ev =
        match GdkEvent.Key.keyval ev with
        | k when k = GdkKeysyms._KP_Subtract -> self#zoom_out (); true
        | k when k = GdkKeysyms._KP_Add -> self#zoom_in (); true
        | _ -> false

      (* Zoom with the mouse *)
      method private zoom_mouse_ev ev =
        match GdkEvent.Scroll.direction ev with
        | `UP -> self#zoom_in (); true
        | `DOWN -> self#zoom_out (); true
        | _ -> false

      method highlight ?color node =
        let h e = e#highlight ?color () in
        h node;
        self#iter_associated_vertex (fun v ->
            h v;
            self#iter_succ_e h v;
            self#iter_pred_e h v)
          node

      method dehighlight node =
        let h e = e#dehighlight () in
        h node;
        self#iter_associated_vertex (fun v ->
            h v;
            self#iter_succ_e h v;
            self#iter_pred_e h v)
          node

      method connect_highlighting_event () =
        let connect node =
          let callback = function
            | `MOTION_NOTIFY _ -> self#highlight node; false
            | `LEAVE_NOTIFY _ -> self#dehighlight node; false
            | _ -> false
          in
          node#connect_event ~callback
        in
        self#iter_nodes connect

      initializer
        (* Create and add items from the model vertices, edges and clusters *)
        model#iter_clusters self#add_cluster;
        model#iter_vertex self#add_vertex;
        model#iter_edges_e self#add_edge;
        (* Scroll region management *)
        ignore $ self#set_center_scroll_region true;
        ignore $ self#set_scroll_region ~x1 ~y1 ~x2 ~y2 ;
        (* Attach zoom events *)
        ignore $ self#event#connect#key_press ~callback:self#zoom_keys_ev;
        ignore $ self#event#connect#scroll ~callback:self#zoom_mouse_ev;

    end

  let view
      ?(aa=false) ?delay_node ?delay_edge ?delay_cluster
      ?border_width ?width ?height ?packing ?show
      (model:(vertex, edge, cluster) DGraphModel.abstract_model) =
    let canvas = 
      GnoCanvas.canvas ~aa ?border_width ?width ?height ?show ?packing () 
    in
    (* Grab focus to process keyboard input *)
    ignore $ canvas#event#connect#enter_notify 
      ~callback:(fun _ -> canvas#misc#grab_focus () ; false); 
    let view = 
      new view ?delay_node ?delay_edge ?delay_cluster
        (Gobject.unsafe_cast canvas#as_widget) 
        model 
    in 
    view 

end
