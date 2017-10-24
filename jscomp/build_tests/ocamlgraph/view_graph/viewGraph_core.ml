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
*)

(** This is Ocamlgraph library : see http://ocamlgraph.lri.fr/doc/ *)
open Graph

exception DotError of string

type t_point = float * float
type t_coord = t_point * t_point

type t_shape = Srect | Sellipse (* TODO : add some more ! *)

type t_gtk_obj = GnomeCanvas.re_p GnoCanvas.item

module Node = struct
  type t = string * (t_shape * t_coord * t_gtk_obj) option
  let id n = fst n
  let coord n = match snd n with None -> None
                               | Some (_, c, _) -> Some c
  let item n = match snd n with None -> None
                              | Some (_, _, r) -> Some r
end
module G = Imperative.Digraph.Abstract(Node)
module B = Builder.I(G)

type t_graph = B.G.t * GnoCanvas.pixbuf
type t_node = B.G.V.t 

let get_graph g = fst g
let get_pixbuf g = snd g

let get_node_info n = B.G.V.label n
let get_coord n = Node.coord (get_node_info n) 
let get_id n =  Node.id (get_node_info n)
let get_obj n = Node.item (get_node_info n) 

(** find the attributes [pos], [width] and [height] in the attribute list *)
let get_info attr_list_list =
  let get (shp, p,w,h) (attr, val_opt) = match attr, val_opt with
    | (Dot_ast.Ident "shape"), Some (Dot_ast.String s) ->
      (* Format.printf "found pos = %s@." s; *)
      (Some s), p, w, h
    | (Dot_ast.Ident "pos"), Some (Dot_ast.String s) ->
      (* Format.printf "found pos = %s@." s; *)
      shp, (Some s), w, h
    | (Dot_ast.Ident "width"), Some (Dot_ast.String s) ->
      (* Format.printf "found width = %s@." s; *)
      shp, p, (Some s), h
    | (Dot_ast.Ident "height"), Some (Dot_ast.String s) ->
      (* Format.printf "found height = %s@." s; *)
      shp, p, w, (Some s)
    | (Dot_ast.Ident _), Some (Dot_ast.String _) ->
      (* Format.printf "found %s = %s -> ignored@." id s; *)
      (shp, p, w, h)
    | _ -> (shp, p,w,h)
  in
  let get acc attr_list = 
    (* Format.printf "%d attr in attr_list@." (List.length attr_list); *)
    List.fold_left get acc attr_list in
  (*Format.printf "%d lists in attr_list_list@." (List.length attr_list_list); *)
  List.fold_left get (None, None, None, None) attr_list_list


(** Translate the information given by dot
 * into the coordinate of a rectangle in the png image.
 * see http://www.graphviz.org/mywiki/FaqCoordTransformation
 *     to understand the [pad] and [factor] variables.
 * @param pos position of the center of the node, in points.
 * @param w width of the node, in inch.
 * @param h height of the node, in inch.
*)
let compute_coord pos w h = 
  let dot_ppi = 72. (* number of pixels per inch on a display device *) in
  let dot_png_ppi = 96. (* number of pixels per inch on a display device *) in
  try
    let w = float_of_string w in
    let h = float_of_string h in
    let x,y = Scanf.sscanf pos "%d,%d" (fun x y -> (x,y)) in
    let pad = 4 in
    let x = float_of_int (x + pad) in
    let y = float_of_int (y + pad) in
    let dx = w *. dot_ppi /. 2. in
    let dy = h *. dot_ppi /. 2. in
    let x1 = x -. dx in
    let y1 = y -. dy in
    let x2 = x +. dx in
    let y2 = y +. dy in
    let factor = dot_png_ppi /. dot_ppi in
    let x1 = x1 *. factor in
    let y1 = y1 *. factor in
    let x2 = x2 *. factor in
    let y2 = y2 *. factor in
    (* Format.printf "compute_coord -> x1=%f y1=%f x2=%f y2=%f@." 
       x1 y1 x2 y2; *)
    Some ((x1,y1),(x2,y2))
  with e -> 
    let s = Printexc.to_string e in
    Format.printf "compute_coord failled : %s@." s;
    None

module DotParser (C : sig val mk_node : t_shape -> t_coord -> t_gtk_obj end) = 
  Dot.Parse
    (B)
    (struct 
      let node (id,_) attr_list = 
        let name = match id with
          | Dot_ast.Ident s
          | Dot_ast.Number s
          | Dot_ast.String s
          | Dot_ast.Html s -> s
        in
        let info = match get_info attr_list with
          | shp, Some pos, Some w, Some h -> 
            let shp = match shp with 
              | Some "ellipse" -> Sellipse 
              | Some "box" -> Srect 
              | Some _ -> Srect 
              | None -> Sellipse (* default shape *)
            in
            begin match compute_coord pos w h with 
              | None -> None 
              | Some coord ->
                let n_obj = C.mk_node shp coord in
                Some (shp, coord, n_obj)
            end 
          | _ -> Format.printf "info KO for %s@." name; None
        in (name, info)
      let edge _ = ()
    end)

(** Call [dot] to build the graph image in a [png] file *)
let png_graph_image dot_cmd dot_file png_file =
  let cmd = Printf.sprintf "%s -T png %s > %s" dot_cmd dot_file png_file in
  match Sys.command cmd with
  | 0 -> png_file
  | _ -> raise (DotError cmd)

(** Call 'dot' on the [dot_file] to get a file with position information,
 * and also to have a [png] image of the graph.
 * Then parse the annotated file to get the graph with the nodes coordinates. 
 * @return the graph and the pgn filename.
 * *)
let build_graph dot_cmd dot_file annot_dot_file mk_node_item = 
  let cmd = Printf.sprintf "%s -y %s > %s " dot_cmd dot_file annot_dot_file in
  match Sys.command cmd with
  | 0 ->
    let module Parser = 
      DotParser (struct let mk_node = mk_node_item end) in
    let graph = Parser.parse annot_dot_file in
    graph
  | _ -> raise (DotError cmd)

(** @return 2 lists : the predecessors and successors of the node*)
let get_neighbours graph n =
  let graph = get_graph graph in
  let preds = B.G.pred graph n in
  let succs = B.G.succ graph n in
  (preds, succs)

(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)

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

module EmptyCb = struct
  type t_env = unit
  let button_one_press_on_graph _env = ()
  let button_two_press_on_graph _env = ()
  let button_three_press_on_graph _env = ()
  let button_one_press_on_node _env _n = ()
  let button_two_press_on_node _env _n = ()
  let button_three_press_on_node _env _n = ()
  let enter_node _env _n = ()
  let leave_node _env _n = ()
end

module M (Cb : SigCb) = struct

  let mk_node_item canvas shp ((x1,y1),(x2,y2)) =
    (* we have to put some color on item in order to be able to get their events,
     * so let's cheat and add a bitmap with 0 every where... *)
    let no_color = [ 
      `FILL_COLOR "black" ;
      `FILL_STIPPLE (Gdk.Bitmap.create_from_data ~width:1 ~height:1 "\000") 
    ] in
    let props = [ `X1 x1; `Y1 y1; `X2 x2; `Y2 y2] @ no_color in 
    let n_obj = match shp with
      | Srect -> GnoCanvas.rect canvas#root ~props
      | Sellipse -> GnoCanvas.ellipse canvas#root ~props
    in n_obj

  let graph_event env ev = 
    begin match ev with
      | `BUTTON_PRESS ev -> 
        begin
          (* let state = GdkEvent.Button.state ev in *)
          match GdkEvent.Button.button ev with
          (* | 1 when Gdk.Convert.test_modifier `SHIFT state ->
             | 1 when Gdk.Convert.test_modifier `CONTROL state -> 
              let (x, y) = 
                canvas#w2c_d (GdkEvent.Button.x ev) (GdkEvent.Button.y ev) in
          *)
          | 1 -> Cb.button_one_press_on_graph env
          | 2 -> Cb.button_two_press_on_graph env
          | 3 -> Cb.button_three_press_on_graph env
          | _ -> ()
        end
      | _ -> () 
    end ; false

  let node_event env node ev =
    begin match ev with
      | `ENTER_NOTIFY _ -> Cb.enter_node env node
      | `LEAVE_NOTIFY _ -> Cb.leave_node env node
      | `BUTTON_PRESS ev -> 
        begin match GdkEvent.Button.button ev with
          | 1 -> Cb.button_one_press_on_node env node
          | 2 -> Cb.button_two_press_on_node env node
          | 3 -> Cb.button_three_press_on_node env node
          | _ -> ()
        end
      | _ -> ()
    end ; 
    false

  (** for each node that has an item, connect the events *)
  let add_node_items env graph =
    let do_it n = 
      match get_obj n with 
      | None -> ()
      | Some n_rect -> ignore (n_rect#connect#event ~callback:(node_event env n))
    in B.G.iter_vertex do_it graph

  let remove_node_items graph =
    let do_it n = 
      match get_obj n with 
      | None -> ()
      | Some n_rect -> n_rect#destroy ()
    in B.G.iter_vertex do_it graph

  let install_image (canvas :GnoCanvas.canvas) png_file =
    let im = GdkPixbuf.from_file png_file in
    (*let im = GdkPixbuf.add_alpha ~transparent:(0xff, 0xff, 0xff) im in*)
    let w = GdkPixbuf.get_width im in
    let h = GdkPixbuf.get_height im in
    (* Format.printf "GnoCanvas.pixbuf size = %dx%d@." w h; *)
    let _ = canvas#set_scroll_region ~x1:0. ~y1:0. ~x2:(float w) ~y2:(float h) in
    let px = GnoCanvas.pixbuf ~x:0. ~y:0. ~pixbuf:im canvas#root in
    px 

  let open_dot_file env (canvas :GnoCanvas.canvas) ?(dot_cmd="dot") dot_file =
    let basename = try Filename.chop_extension dot_file 
      with Invalid_argument _ -> dot_file in
    let png_file = Printf.sprintf "%s.png" basename in
    let annot_dot_file = Printf.sprintf "%s_annot" dot_file in

    let graph = 
      build_graph dot_cmd dot_file annot_dot_file (mk_node_item canvas) in
    (* TODO : it would be better not to recompute the layout, 
     * ie. use annot_dot_file instead of dot_file,
     * but it seems that it doesn't work properly... 
     * It is ok for 'simple' graphs like unix.dot,
     * but not on crazy.dot for instance. What goes wrong ?
     * Anyway, it would be better to build GTK objects instead of a png image !
     * *)
    let png_file = png_graph_image dot_cmd dot_file png_file in
    let pixbuf = install_image canvas png_file in
    let _ = pixbuf#connect#event ~callback:(graph_event env) in
    let _ = add_node_items env graph in 
    let _ = pixbuf#lower_to_bottom () in
    (graph, pixbuf)

  let clear _canvas graph = 
    (* TODO : remove pixbuf from _canvas ? *)
    let pixbuf = get_pixbuf graph in pixbuf#destroy ();
    let graph = get_graph graph in remove_node_items graph

end

