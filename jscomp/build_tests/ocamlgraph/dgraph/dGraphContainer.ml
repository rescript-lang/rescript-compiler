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

let ($) f x = f x

type cluster = string
type status = Global | Tree | Both

(* ABSTRACT CLASS *)

class type
  ['vertex, 'edge, 'cluster, 'tree_vertex, 'tree_edge, 'tree_cluster]
    view_container_type =
  object

    method global_view :
      ('vertex, 'edge, 'cluster) DGraphView.view option

    method tree_view:
      ('tree_vertex, 'tree_edge, 'tree_cluster) DGraphView.view option

    method tree_root: 'vertex option

    method depth_backward : int
    method depth_forward : int
    method status : status

    method set_depth_backward : int -> unit
    method set_depth_forward : int -> unit
    method set_tree_root: 'vertex -> unit
    method switch : status -> unit

    method adapt_zoom: unit -> unit

  end

module type S = sig

  type graph
  type vertex
  type edge

  module Tree: Sig.G with type V.label = vertex

  module GView: DGraphView.S with type vertex = vertex
                              and type edge = edge
                              and type cluster = cluster

  module TView: DGraphView.S with type vertex = Tree.V.t
                              and type edge = Tree.E.t
                              and type cluster = cluster

  type global_view = (vertex, edge, cluster) DGraphView.view
  type tree_view = (Tree.V.t, Tree.E.t, cluster) DGraphView.view

  class view_container :
    ?packing:(GObj.widget -> unit)
    -> ?status:status
    -> ?default_callbacks:bool
    -> mk_global_view: (unit -> global_view)
    -> mk_tree_view:
    (depth_backward:int -> depth_forward:int -> Gtk.widget Gtk.obj -> vertex
     -> tree_view)
    -> vertex option
    -> [ vertex, edge, cluster, Tree.V.t, Tree.E.t, cluster]
      view_container_type

end

(* CONTAINER *)

let with_commands ?packing mk_view model =
  let main_table = GPack.table ~columns:2 ~rows:2 ?packing () in
  let view =
    let packing w = main_table#attach ~left:0 ~right:2 ~top:1 ~expand:`BOTH w in
    mk_view ~packing model
  in
  (* Buttons box *)
  let buttons_box =
    GPack.button_box
      `HORIZONTAL
      ~border_width:3
      ~child_height:10
      ~child_width:85
      ~spacing:10
      ~layout:`START
      ~packing:(main_table#attach ~top:0 ~left:0 ~expand:`X) ()
  in
  let view_label = GMisc.label ~markup:"<b>View</b>" () in
  buttons_box#pack ~expand:false view_label#coerce;
  let add_button label mode =
    let b = GButton.button ~label ~packing:buttons_box#pack () in
    ignore (b#connect#clicked ~callback:(fun _ -> view#switch mode))
  in
  add_button "Global" Global;
  add_button "Tree" Tree;
  add_button "Both" Both;

  (* Depth of exploration controls *)
  let depth_hbox =
    GPack.hbox ~packing:(main_table#attach ~expand:`X ~top:0 ~left:1) ()
  in
  let depth_forward_adj = GData.adjustment ~lower:0. ~page_size:0. () in
  let depth_backward_adj = GData.adjustment ~lower:0. ~page_size:0. () in
  let change_depth_forward adj content () =
    content#set_depth_forward (int_of_float adj#value);
  in
  let change_depth_backward adj content () =
    content#set_depth_backward (int_of_float adj#value)
  in
  ignore $ depth_forward_adj#connect#value_changed
    ~callback:(change_depth_forward depth_forward_adj view);
  ignore $ depth_backward_adj#connect#value_changed
    ~callback:(change_depth_backward depth_backward_adj view);
  let depth_label = GMisc.label ~markup:"<b>Depth</b>" () in
  let depth_forward_label = GMisc.label ~text:" forward: " () in
  let depth_backward_label = GMisc.label ~text:" backward: " () in
  let depth_forward_spin =
    GEdit.spin_button ~value:2. ~adjustment:depth_forward_adj ()
  in
  let depth_backward_spin =
    GEdit.spin_button ~value:2. ~adjustment:depth_backward_adj ()
  in
  depth_hbox#pack ~from:`END depth_backward_spin#coerce;
  depth_hbox#pack ~from:`END depth_backward_label#coerce;
  depth_hbox#pack ~from:`END depth_forward_spin#coerce;
  depth_hbox#pack ~from:`END depth_forward_label#coerce;
  depth_hbox#pack ~from:`END depth_label#coerce;

  main_table, view

(* FROM GRAPH *)

module HString = struct
  type t = string
  let equal = (=)
  let hash = Hashtbl.hash
end

module Build
    (G: Sig.G)
    (T: DGraphTreeModel.S with type Tree.V.label = G.V.t) =
struct

  type graph = G.t
  type vertex = G.V.t
  type edge = G.E.t
  module TreeModel = T
  module Tree = T.Tree

  module HE(E: sig type t val compare: t -> t -> int end) = struct
    type t = E.t
    let equal x y = E.compare x y = 0
    let hash = Hashtbl.hash
  end
  module GView = DGraphView.Make(G.V)(HE(G.E))(HString)
  module TView = DGraphView.Make(Tree.V)(HE(Tree.E))(HString)

  type global_view = (G.V.t, G.E.t, string) DGraphView.view
  type tree_view = (Tree.V.t, Tree.E.t, string) DGraphView.view

  class view_container
      ?packing
      ?(status=Global)
      ?(default_callbacks=true)
      ~mk_global_view
      ~mk_tree_view
      default_tree_root
    =

    (* widgets *)
    let paned_box = GPack.paned `VERTICAL ?packing () in
    let global_frame = GBin.frame ~label:"Global View" () in
    let tree_frame = GBin.frame ~label:"Tree View" () in

    (* Callback functions *)
    let connect_tree_callback view node =
      let item = node#item in
      let callback ev =
        let apply f = match view#global_view with
          | None -> false
          | Some gview ->
            let tree = T.tree () in
            let vlist = T.TreeManipulation.get_graph_vertex item tree in
            f (gview:global_view) (gview#get_node vlist);
            false
        in
        match ev with
        | `BUTTON_PRESS _ ->
          (* clicking on a node of the tree view *)
          view#set_tree_root (T.Tree.V.label item);
          apply (fun v -> v#dehighlight)
        | `MOTION_NOTIFY _ -> apply (fun v n -> v#highlight n)
        | `LEAVE_NOTIFY _ -> apply (fun v -> v#dehighlight)
        | _ -> false
      in
      node#connect_event ~callback
    in
    let connect_global_callback view node =
      let item = node#item in
      let callback ev =
        let apply f = match view#tree_view with
          | None -> false
          | Some tview ->
            let tree = T.tree () in
            let vlist = T.TreeManipulation.get_tree_vertices item tree in
            List.iter (fun v -> f (tview:tree_view) (tview#get_node v)) vlist;
            false
        in
        match ev with
        | `BUTTON_PRESS _ -> (* clicking on a node of the global view *)
          (* Update the root of the tree view *)
          view#set_tree_root node#item;
          (* Center the global view on the selected node *)
          begin match view#global_view with 
            | None -> ()
            | Some w -> w#center_node node
          end;
          false
        | `MOTION_NOTIFY _ -> apply (fun v n -> v#highlight n)
        | `LEAVE_NOTIFY _ -> apply (fun v -> v#dehighlight)
        |_ -> false
      in
      node#connect_event ~callback
    in

    object (self)

      val mutable global_view: global_view option = None
      val mutable tree_view: tree_view option = None
      val mutable tree_change = false (* must recompute the tree? *)
      val mutable tree_root = default_tree_root
      val mutable status = status
      val mutable depth_forward = 2
      val mutable depth_backward = 2

      (* Getters *)

      method status = status
      method global_view = global_view
      method tree_view = tree_view
      method tree_root = tree_root
      method depth_forward = depth_forward
      method depth_backward = depth_backward

      (* Setters *)

      method set_depth_forward i =
        depth_forward <- i;
        tree_change <- true;
        self#update ()

      method set_depth_backward i =
        depth_backward <- i;
        tree_change <- true;
        self#update ()

      method set_tree_root v =
        tree_root <- Some v;
        tree_change <- true;

        self#update ()

      method switch s =
        status <- s;
        self#update ()

      (* Compute the views *)

      method private memo_tree_view () =
        if tree_change then begin
          (match tree_view with
           | None -> ()
           | Some view ->
             match view#misc#parent with
             | Some parent -> tree_frame#remove parent
             | None -> ()
          );
          match tree_root with
          | None -> assert false
          | Some r ->
            let view =
              mk_tree_view ~depth_backward ~depth_forward paned_box#as_widget r
            in
            tree_view <- Some view;
            if default_callbacks then begin
              view#connect_highlighting_event();
              view#iter_nodes (connect_tree_callback self);
            end;
            let scroll =
              GBin.scrolled_window
                ~hpolicy:`AUTOMATIC
                ~vpolicy:`AUTOMATIC
                ~packing:tree_frame#add
                ()
            in
            scroll#add view#coerce
        end

      method private memo_global_view () = match global_view with
        | None ->
          let view = mk_global_view () in
          global_view <- Some view;
          if default_callbacks then begin
            view#connect_highlighting_event ();
            view#iter_nodes (connect_global_callback self);
          end;
          let scroll =
            GBin.scrolled_window
              ~hpolicy:`AUTOMATIC
              ~vpolicy:`AUTOMATIC
              ~packing:global_frame#add
              ()
          in
          scroll#add view#coerce
        | Some _ ->
          ()

      method adapt_zoom () =
        let az = function
          | None -> ()
          | Some w -> w#adapt_zoom ()
        in
        az tree_view;
        az global_view

      method private add_global_view () =
        self#memo_global_view ();
        paned_box#pack1 global_frame#coerce

      method private add_tree_view () =
        self#memo_tree_view ();
        paned_box#pack2 tree_frame#coerce

      method private update () =
        List.iter paned_box#remove paned_box#all_children;
        (match status, tree_root with
         | Global, _ | _, None -> self#add_global_view ()
         | Tree, Some _ -> self#add_tree_view ()
         | Both, Some _ -> self#add_global_view (); self#add_tree_view ())

      initializer
        self#update ()

    end

end

module Make(G: Graphviz.GraphWithDotAttrs) = struct

  module FullTreeModel = DGraphTreeModel.SubTreeMake(G)
  include Build(G)(FullTreeModel)
  module GlobalModel = DGraphModel.Make(G)

  let from_graph
      ?packing
      ?status
      ?(default_callbacks=true)
      ?(mk_global_view = fun model -> GView.view ~aa:true model)
      ?(mk_tree_view = fun model -> TView.view ~aa:true model)
      ?root
      g =
    let status = match status with
      | None ->
        if G.nb_vertex g < 500 && G.nb_edges g < 2500 then Global else Tree
      | Some s -> s
    in
    new view_container
      ?packing
      ~default_callbacks
      ~status
      ~mk_global_view:(fun () -> mk_global_view (GlobalModel.from_graph g))
      ~mk_tree_view:(fun ~depth_backward ~depth_forward w v ->
          let model =
            FullTreeModel.from_graph ~depth_forward ~depth_backward w g v
          in
          mk_tree_view model)
      root

  let from_graph_with_commands ?packing ?status ?(default_callbacks=true) ?mk_global_view ?mk_tree_view ?root g =
    with_commands
      ?packing
      (fun ~packing g ->
         from_graph
           ~packing ?status ~default_callbacks ?mk_global_view ?mk_tree_view
           ?root g)
      g

end

(* FROM DOT *)

module Dot = struct

  include Build(DGraphModel.DotG)(DGraphTreeModel.SubTreeDotModelMake)

  exception Found of DGraphModel.DotG.V.t

  let from_dot
      ?packing
      ?status
      ?(default_callbacks=true)
      ?(mk_global_view = fun model -> GView.view ~aa:true model)
      ?(mk_tree_view = fun model -> TView.view ~aa:true model)
      dot_file =
    let gmodel =
      if Filename.check_suffix dot_file "xdot" then
        DGraphModel.read_xdot dot_file
      else
        DGraphModel.read_dot dot_file
    in
    let one_vertex =
      try
        gmodel#iter_vertex (fun v -> raise (Found v));
        None
      with Found v ->
        Some v
    in
    let status = match status with
      | None ->
        let nb f =
          let cpt = ref 0 in
          f gmodel (fun _ -> incr cpt);
          !cpt
        in
        if nb (fun g -> g#iter_vertex) < 500
        && nb (fun g -> g#iter_edges_e) < 2500
        then Global
        else Tree
      | Some s -> s
    in
    new view_container
      ?packing
      ~status
      ~default_callbacks
      ~mk_global_view:(fun () -> mk_global_view gmodel)
      ~mk_tree_view:(fun ~depth_backward ~depth_forward _ v ->
          mk_tree_view
            (DGraphTreeModel.SubTreeDotModelMake.from_model
               ~depth_forward
               ~depth_backward
               gmodel
               v))
      one_vertex

  let from_dot_with_commands ?packing ?status ?(default_callbacks=true) ?mk_global_view ?mk_tree_view dot_file =
    with_commands
      ?packing
      (fun ~packing d ->
         from_dot
           ~packing ?status ~default_callbacks ?mk_global_view ?mk_tree_view d)
      dot_file

end
