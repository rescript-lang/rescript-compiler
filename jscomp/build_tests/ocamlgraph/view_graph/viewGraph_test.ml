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

(** This is only a test file to show how to use 
 * [ViewGraph] and [ViewGraph_select].
 * Just compile and test... (click on the Help button to know how to use it).
*)

(* Nice tutorial at :
 * http://plus.kaist.ac.kr/~shoh/ocaml/lablgtk2/lablgtk2-tutorial/book1.html
 * See also examples in :
 *  /usr/share/doc/lablgtk-2.6.0/examples/canvas/
*)

(** To test to callbacks : only print messages *)
module CbTest = struct
  (** need nothing for this test, but usually contains at least the graph... *)
  type t_env = unit 

  let button_one_press_on_graph _env = 
    Format.printf "[CbTest] button_one_press_on_graph @."

  let button_two_press_on_graph _env = 
    Format.printf "[CbTest] button_two_press_on_graph @."

  let button_three_press_on_graph _env = 
    Format.printf "[CbTest] button_three_press_on_graph @."

  let button_one_press_on_node _env n = 
    Format.printf "[CbTest] button_one_press_on_node %s@." 
      (ViewGraph.get_id n)

  let button_two_press_on_node _env n = 
    Format.printf "[CbTest] button_two_press_on_node %s@." 
      (ViewGraph.get_id n)

  let button_three_press_on_node _env n = 
    Format.printf "[CbTest] button_three_press_on_node %s@." 
      (ViewGraph.get_id n)

  let enter_node _env n =
    Format.printf "[CbTest] enter_node %s@." (ViewGraph.get_id n)

  let leave_node _env n =
    Format.printf "[CbTest] leave_node %s@." (ViewGraph.get_id n)
end

module V = ViewGraph_select.VG (CbTest)

let open_file select_init_env file =
  try 
    let env = () in
    let _graph = V.open_dot_file env select_init_env file in 
    ()
  with ViewGraph.DotError cmd ->
    GToolbox.message_box "Error" 
      (Printf.sprintf 
         "%s failed\nDidn't succed to build graph for %s\nSorry !" cmd file)

let open_cb select_init_env () = 
  match GToolbox.select_file ~title:"Select a dot file" () with
  | None -> ()
  | Some filename -> open_file select_init_env filename

let help_act_cb _ac = ViewGraph_select.show_help ()

let error_act_cb ac = 
  GToolbox.message_box "Error" 
    (Printf.sprintf "Action '%s' activated : no callback ?\n" ac#name) 

let quit_cb () = GMain.Main.quit ()

let quit_act_cb _a = quit_cb ()

let menu_desc = "<ui>\
                 <menubar name='MenuBar'>\
                 <menu action='FileMenu'>\
                 <menuitem action='Open'/>\
                 <separator/>\
                 <menuitem action='Quit'/>\
                 </menu>\
                 <menu action='HelpMenu'>\
                 <menuitem action='Help'/>\
                 </menu>\
                 </menubar>\
                 </ui>"

let create_menu () =
  let ui_m = GAction.ui_manager () in
  let actions = GAction.action_group ~name:"Actions" () in
  GAction.add_actions actions [
    GAction.add_action "FileMenu" ~label:"File" ;
    GAction.add_action "Open" ~label:"Open" ~accel:"o" 
    (* callback connected later *);
    GAction.add_action "Quit" ~label:"Quit" ~accel:"q" ~callback:quit_act_cb;
    GAction.add_action "HelpMenu" ~label:"Help" ;
    GAction.add_action "Help" ~label:"Help" ~accel:"h" ~callback:help_act_cb;
  ];
  ui_m#insert_action_group actions 0 ;
  let _ = ui_m#add_ui_from_string menu_desc in

  let help_item = ui_m#get_widget "/MenuBar/HelpMenu" in
  let help_item =  GtkMenu.MenuItem.cast help_item#as_widget in
  GtkMenu.MenuItem.set_right_justified help_item true ;
  ui_m

      (*
let create_menu2 packing open_cb =
  let file_item = GMenu.menu_item ~label:"File" ~packing () in
  let file_menu = GMenu.menu () in
  let item = GMenu.menu_item ~label:"Open" ~packing:file_menu#append () in
  let _ = item#connect#activate ~callback:open_cb in
  let item = GMenu.menu_item ~label:"Quit" ~packing:file_menu#append () in
  let _ = item#connect#activate ~callback:GMain.Main.quit in
  let _ = file_item#set_submenu file_menu in
  let help_item = GMenu.menu_item ~label:"Help" 
                    ~right_justified:true ~packing () in 
  let help_menu = GMenu.menu () in
  let item = GMenu.menu_item ~label:"Help about ViewGraph" 
               ~packing:help_menu#append () in
  let _ = item#connect#activate ~callback:ViewGraph_select.show_help in
  let _ = help_item#set_submenu help_menu in
    ()
    *)


let create_gui () =
  let window = GWindow.window ~title:"ViewGraph" 
      ~allow_shrink:true  ~allow_grow:true ()  in
  let vbox = GPack.vbox ~border_width:4 ~spacing:4 ~packing:window#add () in

  let ui_m = create_menu () in
  window#add_accel_group ui_m#get_accel_group ;
  vbox#pack ~expand:false (ui_m#get_widget "/MenuBar") ;

  let frame = GBin.frame ~label:"How to use this :" ~packing:vbox#pack () in
  let _ = GMisc.label ~text:"\n Open the Help window to know more...\n"
      ~packing:frame#add () in

  let pack = vbox#pack ~expand:true ~fill:true in
  let canvas = ViewGraph_utils.create_scrolled_canvas pack in

  let hbox = GPack.hbox ~spacing:4 ~packing:vbox#pack () in
  let select_init_env = 
    ViewGraph_select.init ViewGraph_select.default_options
      canvas (hbox#pack ~expand:true ~fill:true) in

  let actions = match ui_m#get_action_groups with
    | a::[] -> a | _ -> assert false
  in
  let open_action = actions#get_action "Open" in 
  let _ = open_action#connect#activate ~callback:(open_cb select_init_env) in

  let _ = window#connect#destroy ~callback:quit_cb in
  let _ = window#show () in
  (canvas, select_init_env)

let main () =
  let _ = GMain.Main.init () in
  let canvas, select_init_env = create_gui () in
  if Array.length Sys.argv = 2 then
    open_file select_init_env Sys.argv.(1);
  GMain.Main.main ()

let _ = Printexc.print main ()

