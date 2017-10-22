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

(** This file provide useful function to build windows to put the graph *)

let create_scrolled_canvas packing =
  let frame = GBin.frame ~shadow_type:`IN () in
  let canvas = 
    let aa = false (* anti-aliasing *) in
    GnoCanvas.canvas ~aa ~width:600 ~height:400 ~packing:frame#add () 
  in
  let _ = canvas#set_center_scroll_region true in
  (* if the graph is too big, show its center *)
  let table = GPack.table ~packing
      ~rows:2 ~columns:2 ~row_spacings:4 ~col_spacings:4 () in
  let _ = table#attach ~left:0 ~right:1 ~top:0 ~bottom:1
      ~expand:`BOTH ~fill:`BOTH ~shrink:`BOTH ~xpadding:0 ~ypadding:0
      frame#coerce in
  let w = GRange.scrollbar `HORIZONTAL ~adjustment:canvas#hadjustment ()  in
  let _ = table#attach ~left:0 ~right:1 ~top:1 ~bottom:2
      ~expand:`X ~fill:`BOTH ~shrink:`X ~xpadding:0 ~ypadding:0
      w#coerce  in
  let w = GRange.scrollbar `VERTICAL ~adjustment:canvas#vadjustment ()  in
  let _ = table#attach ~left:1 ~right:2 ~top:0 ~bottom:1
      ~expand:`Y ~fill:`BOTH ~shrink:`Y ~xpadding:0 ~ypadding:0 
      w#coerce  in
  canvas

let create_graph_win title =
  let window = GWindow.window ~title
      ~allow_shrink:true  ~allow_grow:true ()  in
  let vbox = GPack.vbox ~border_width:4 ~spacing:4 ~packing:window#add () in
  let help_but = GButton.button ~label:"Help"
      ~packing:(vbox#pack ~expand:false ~fill:true) () in
  let _ = help_but#connect#clicked ~callback:ViewGraph_select.show_help in
  let canvas = create_scrolled_canvas (vbox#pack ~expand:true ~fill:true) in
  let hbox = GPack.hbox ~spacing:4 ~packing:vbox#pack () in
  let select_init_env =
    ViewGraph_select.init ViewGraph_select.default_options
      canvas (hbox#pack ~expand:true ~fill:true) in
  window#show ();
  select_init_env

