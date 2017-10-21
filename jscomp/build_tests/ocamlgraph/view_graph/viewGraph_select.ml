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

(** This module can be used to add some selection feature to [ViewGraph].
 * see [show_help] for more details. *)

(** To manage the lists of neighbours of the selected node.
 * The type [t] is made of a boolean [b] and 3 lists [(l1, l2, l3)].
 * [b] tells is [l1] is a list of predecessors (vs. successors).
 * [l3] has the same 'kind' than [l1], and [l2] the other.
 * [l1] cannot be empty, except if the 3 lists are empty.
 * The first node of [l1] is the currently selected neighbour.
 * When we go to the next one, [n] is removed and appended to [l3] tail.
 * If [l1] is then empty, [l2] becomes the first list, and [b] is changed. *)
module CircLists = struct
  type t_elem = ViewGraph_core.t_node 
  type t_lists = t_elem list * t_elem list * t_elem list
  type t = bool * t_lists

  let mk (l1, l2) = match l1, l2 with
    | [], [] -> true, ([], [], [])
    | [], l2 -> false, (l2, [], [])
    | l1, l2 -> true, (l1, l2, [])

  let current_node lists = match lists with
    | _, ( [] , _, _)  -> None
    | b, (n::_, _, _) -> Some (b, n)

  let go_next lists = 
    let lists = match lists with 
      | _, ([], [], []) -> lists
      | _, ([], _, _) -> 
        assert false (* l1 cannot be empty, except if every lists are *)
      | b, (n::[], [], l3) -> b, (l3 @ [n], [], [])
      | b, (n::[], l2, l3) -> not b, (l2, l3 @ [n], [])
      | b, (n::l1, l2, l3) -> b, (l1, l2, l3 @ [n])
    in lists

  let goto_n ((_, (l1, l2, l3)) as lists) n =
    let max = List.length l1 + List.length l2 + List.length l3 in
    let rec find nb_iter lists =
      match current_node lists with
      | None -> raise Not_found
      | Some (_, cur_n) ->
        if  cur_n = n then lists
        else if nb_iter >= 0 then
          find (nb_iter - 1) (go_next lists)
        else raise Not_found
    in find max lists

end

type t_options = {
  sel_1_color : string;
  sel_2_color : string;
  center_node_when_selected :bool;
}

let default_options = {
  sel_1_color = "red";
  sel_2_color = "green";
  center_node_when_selected = true;
}

(** Some widgets, useful for the selection. *)
type t_widgets = {
  canvas : GnoCanvas.canvas;
  sel_1_txt : GMisc.label;
  sel_edge_txt : GMisc.label;
  sel_2_txt : GMisc.label;
}

type t_state = { 
  w : t_widgets;
  opt : t_options;
  mutable graph : ViewGraph_core.t_graph option ;
  mutable ppu : float ; 
  mutable selected : ViewGraph_core.t_node option ;
  mutable neighbours : CircLists.t option;
  mutable gui_sel : (ViewGraph_core.t_gtk_obj option) array;
}

type t_env = t_state

(* horrible zoom : probably to be changed ! *)
let change_zoom state delta =
  state.ppu <- state.ppu +. delta;
  state.w.canvas#set_pixels_per_unit state.ppu

let center_selected_node state =
  match state.selected with
  | None -> ()
  | Some n -> match ViewGraph_core.get_coord n with
    | None -> ()
    | Some ((x1, y1), (x2, y2)) -> 
      let canvas = state.w.canvas in
      let x = x1 +. (x2 -. x1) /. 2. in
      let y = y1 +. (y2 -. y1) /. 2. in
      (* Format.printf "want to see = %f x %f@." x y; *)
      let w = canvas#hadjustment#page_size in
      let h = canvas#vadjustment#page_size in
      (* Format.printf "page_size = %f x %f@." w h; *)
      let sx = x -. (w /. 2.) in
      let sy = y -. (h /. 2.) in
      (* Format.printf "scroll to = %f x %f (world) @." sx sy; *)
      let sx, sy = canvas#w2c ~wx:sx ~wy:sy in
      (* this is to handle zoom factor *)
      (* Format.printf "scroll to = %d x %d (canvas) @." sx sy; *)
      canvas#scroll_to ~x:sx ~y:sy

let key_press state ev =
  let canvas = state.w.canvas in
  let (x, y) = canvas#get_scroll_offsets in
  match GdkEvent.Key.keyval ev with
  | k when k = GdkKeysyms._Up -> canvas#scroll_to ~x ~y:(y-20) ; true
  | k when k = GdkKeysyms._Down -> canvas#scroll_to ~x ~y:(y+20) ; true
  | k when k = GdkKeysyms._Left -> canvas#scroll_to ~x:(x-10) ~y ; true
  | k when k = GdkKeysyms._Right -> canvas#scroll_to ~x:(x+10) ~y ; true
  | k when k = GdkKeysyms._Page_Down -> change_zoom state 0.1; true
  | k when k = GdkKeysyms._Page_Up -> change_zoom state (-0.1); true
  | k when k = GdkKeysyms._Home -> center_selected_node state; true
  | _ -> false

let init options (canvas : GnoCanvas.canvas) pack_txt = 
  let _ = GMisc.label ~text:"Selected : " ~packing:pack_txt () in
  let sel_1_txt = GMisc.label ~packing:pack_txt () in
  let sel_edge_txt = GMisc.label ~packing:pack_txt () in
  let sel_2_txt = GMisc.label ~packing:pack_txt () in
  let w = { canvas = canvas;
            sel_1_txt = sel_1_txt; sel_edge_txt = sel_edge_txt;
            sel_2_txt = sel_2_txt } in
  let state = 
    { w = w; opt = options; gui_sel = [| None; None |] ;
      graph = None ; ppu = 1.; selected = None; neighbours = None } in
  let _ = canvas#event#connect#after#key_press ~callback:(key_press state) in
  state

let sel_1_bitmap () = Gdk.Bitmap.create_from_data ~width:2 ~height:2 "\002\001"
let sel_2_bitmap () = Gdk.Bitmap.create_from_data ~width:2 ~height:2 "\002\001"

(* we have to put some color on item in order to be able to get their events,
 * so let's cheat and add a bitmap with 0 every where... *)
let no_bitmap () = Gdk.Bitmap.create_from_data ~width:1 ~height:1 "\000"


let move_selection state n_opt1 n_opt2 =
  let set_props o col bitmap = match o with None -> ()
                                          | Some o -> o#set [ `FILL_COLOR col ; `FILL_STIPPLE bitmap]
  in
  let n_obj n_opt  = match n_opt with
    | None -> None
    | Some n ->  ViewGraph_core.get_obj n 
  in
  let obj1 = n_obj n_opt1 in
  let obj2 = n_obj n_opt2 in
  let reset_old i = set_props state.gui_sel.(i) "black" (no_bitmap()) in
  reset_old 0; reset_old 1;
  state.gui_sel.(0) <- obj1; state.gui_sel.(1) <- obj2;
  set_props obj1 state.opt.sel_1_color (sel_1_bitmap()) ;
  set_props obj2 state.opt.sel_2_color (sel_2_bitmap())

let show_selection state =
  let n_opt, txt1, txt2, txt3 = match state.selected with
    | None -> None, "(none)", "", ""
    | Some n -> 
      let txt1 = ViewGraph_core.get_id n in
      let n_opt, txt2, txt3 = match state.neighbours with 
        | None -> None, "", ""
        | Some info ->
          match CircLists.current_node info with
          | None -> None, " --- ", "(none)"
          | Some (pred_first, n) ->
            let txt2 = if pred_first then "  <--  " else "  -->  " in
            Some n, txt2, ViewGraph_core.get_id n
      in n_opt, txt1, txt2, txt3
  in
  state.w.sel_1_txt#set_text txt1;
  state.w.sel_edge_txt#set_text txt2;
  state.w.sel_2_txt#set_text txt3;
  move_selection state state.selected n_opt;
  if state.opt.center_node_when_selected then
    center_selected_node state

let select_node state n_opt =
  state.selected <- n_opt;
  state.neighbours <- None;
  show_selection state

let select_neighbour state =
  match state.graph with None -> () 
                       | Some graph -> 
                         match state.selected with None -> ()
                                                 | Some n ->
                                                   let new_info = 
                                                     match state.neighbours with 
                                                     | None -> 
                                                       let neighbours = ViewGraph_core.get_neighbours graph n in
                                                       CircLists.mk neighbours
                                                     | Some info -> CircLists.go_next info
                                                   in
                                                   state.neighbours <- Some (new_info);
                                                   show_selection state

let goto_neighbour state = 
  match state.graph with None -> () 
                       | Some graph -> 
                         match state.neighbours with
                         | None -> ()
                         | Some info -> match CircLists.current_node info with 
                           | None -> ()
                           | Some (_, n) -> 
                             match state.selected with 
                             | None -> 
                               (* neighbours without selected node ? Impossible... *)
                               assert false
                             | Some old_n ->
                               state.selected <- Some n;
                               let neighbours = ViewGraph_core.get_neighbours graph n in
                               let neighbours = CircLists.mk neighbours in
                               let neighbours = CircLists.goto_n neighbours old_n in
                               state.neighbours <- Some neighbours;
                               show_selection state

let clear_state state =
  state.graph <- None;
  state.ppu <- 1.; 
  state.selected <- None; 
  state.neighbours <- None;
  state.gui_sel <- [| None; None |];
  show_selection state

module SelectCb (UserCb : ViewGraph_core.SigCb) = struct
  type t_env = UserCb.t_env * t_state

  let button_one_press_on_graph (u_env, state) = 
    select_node state None;
    UserCb.button_one_press_on_graph (u_env)

  let button_two_press_on_graph (u_env, state) = 
    select_neighbour state;
    UserCb.button_two_press_on_graph (u_env)

  let button_three_press_on_graph (u_env, state) = 
    goto_neighbour state;
    UserCb.button_three_press_on_graph (u_env)

  let button_one_press_on_node (u_env, state) n = 
    select_node state (Some n);
    UserCb.button_one_press_on_node (u_env) n

  let button_two_press_on_node ((u_env, _state) as env) n = 
    button_two_press_on_graph env;
    UserCb.button_two_press_on_node (u_env) n

  let button_three_press_on_node ((u_env, _state) as env) n = 
    button_three_press_on_graph env;
    UserCb.button_three_press_on_node (u_env) n

  let enter_node (u_env, _state) n =
    begin match ViewGraph_core.get_obj n with None -> assert false
                                            | Some n_item -> n_item#set [`OUTLINE_COLOR "red"]
    end;
    UserCb.enter_node (u_env) n

  let leave_node (u_env, _state) n =
    begin match ViewGraph_core.get_obj n with None -> assert false
                                            | Some n_item -> n_item#set [`NO_OUTLINE_COLOR]
    end;
    UserCb.leave_node (u_env) n
end

module VG (UserCb : ViewGraph_core.SigCb) = struct
  module Cb = SelectCb (UserCb)
  module V = ViewGraph_core.M (Cb)

  let open_dot_file u_env state ?(dot_cmd="dot") file =
    let canvas = state.w.canvas in
    (match state.graph with None -> () | Some g -> V.clear canvas g);
    let _ = clear_state state in
    let env = (u_env, state) in
    let graph = V.open_dot_file env canvas ~dot_cmd file in
    state.graph <- Some graph;
    graph

end

let show_help () = 
  GToolbox.message_box ~title:"Help"
    "Selection :\n
  - use the mouse button-1 to select a node : \
     it should turn red and (optionally) be centered.
  - when a node is selected, button-2 selects one of its neighbour : \
     it should turn green.
  - using button-2 again selects the next neighbour, and so on.
  - when a neighbour is selected, button-3 makes it the selected node,
       and the previously selected node become the selected neighbour,
       so pressing the button-3 again brings back to it.
  - button-1 outside a node deselect everything.
  Have a look at the bottom line to see the selection.

  Moving arround :\n
  - the scrollbars, as well as the arrows, can be used to move in the window.
  - the Page-Up and Page-Down keys can be used to zoom \
     (ugly zoom at the moment !).
  - the Home key centers the selected node in the window.
  " 
