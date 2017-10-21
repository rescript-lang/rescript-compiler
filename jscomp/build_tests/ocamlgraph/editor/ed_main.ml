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

(* This file is a contribution of Benjamin Vadon *)

open Format

open Ed_hyper
open Ed_graph
open Ed_display

let debug = ref false
let trace f x = 
  try f x with e -> eprintf "TRACE: %s@." (Printexc.to_string e); raise e

let _ = GMain.Main.init ()

(* Model for the treeview on the left *)

module Model = struct

  open Gobject.Data

  let cols = new GTree.column_list
  let name = cols#add string
  let vertex = cols#add caml
  let model = GTree.tree_store cols
  let rows = H.create 97


  let find_row v =
    try 
      H.find rows v
    with Not_found -> 
      Format.eprintf "anomaly: no model row for %s@." (string_of_label v);
      raise Not_found

  let add_vertex v =
    let row = model#append () in
    model#set ~row ~column:name (string_of_label v);
    model#set ~row ~column:vertex v;
    H.add rows v row;
    row

  let add_edge_1 row_v w =
    let row = model#append ~parent:row_v () in
    model#set ~row ~column:name (string_of_label w);
    model#set ~row ~column:vertex w

  let add_edge v w =
    let row_v = find_row v in
    add_edge_1 row_v w;
    if not G.is_directed then 
      let row_w = find_row w in
      add_edge_1 row_w v

  let find_children row_v w =
    let nb_child = model#iter_n_children (Some row_v) in
    let rec find n = 
      let child = model#iter_children ~nth:(n-1) (Some row_v) in
      let child_vertex = model#get ~row:child ~column:vertex  in
      match n with
      | 0 -> raise Not_found
      | n -> 
        if (G.V.equal child_vertex  w)
        then child
        else find (n-1)
    in
    find nb_child

  let remove_edge_1 row_v w =
    ignore (model#remove (find_children row_v w))

  let remove_edge v w =
    let row_v = find_row v in
    remove_edge_1 row_v w;
    if not G.is_directed then 
      let row_w = find_row w in
      remove_edge_1 row_w v

  let remove_vertex vertex = 
    G.iter_succ (fun w -> remove_edge w vertex) !graph vertex;
    let row = find_row vertex in
    model#remove row

  let reset () =
    H.clear rows;
    model#clear ();
    G.iter_vertex
      (fun v -> 
         let row = add_vertex v in
         G.iter_succ (add_edge_1 row) !graph v)
      !graph

end



let () = Model.reset ()

let ed_name = "Ocamlgraph's Editor"


(* Main GTK window *)
let window = GWindow.window ~border_width: 10 ~position: `CENTER () 


(* usual function to change window title *)
let set_window_title () =
  window#set_title
    (match !graph_name with
     | None -> ed_name
     | Some name -> ed_name^" : "^(Filename.chop_extension (Filename.basename name)))


(* menu bar *)
let v_box = GPack.vbox ~homogeneous:false ~spacing:30  ~packing:window#add ()
let menu_bar_box = GPack.vbox ~packing:v_box#pack () 

(* treeview on the left, canvas on the right *)
let h_box = GPack.hbox ~homogeneous:false ~spacing:30  ~packing:v_box#add ()

let sw = GBin.scrolled_window ~shadow_type:`ETCHED_IN ~hpolicy:`NEVER
    ~vpolicy:`AUTOMATIC ~packing:h_box#add () 

let canvas =  
  GnoCanvas.canvas 
    ~aa:!aa 
    ~width:(truncate w) 
    ~height:(truncate h) 
    ~packing:h_box#add () 

(* unit circle as root of graph drawing *)
let canvas_root =
  let circle_group = GnoCanvas.group ~x:300.0 ~y:300.0 canvas#root in
  circle_group#lower_to_bottom ();
  let w2 = 2. in
  let circle = GnoCanvas.ellipse  ~props:[ `X1 (-.w/.2. +.w2); `Y1 (-.h/.2. +.w2); 
                                           `X2  (w/.2. -.w2) ; `Y2 ( h/.2. -.w2) ;
                                           `FILL_COLOR color_circle ; `OUTLINE_COLOR "black" ; 
                                           `WIDTH_PIXELS (truncate w2) ] circle_group 
  in
  circle_group#lower_to_bottom ();
  circle#show();
  let graph_root = GnoCanvas.group ~x:(-.300.0) ~y:(-.300.0) circle_group in
  graph_root#raise_to_top ();
  set_window_title ();
  graph_root


(* current root used for drawing *)
let root = ref (choose_root ())


let load_graph f =
  Ed_graph.load_graph f;
  Model.reset ();
  set_window_title ();
  root := choose_root ()


(* refresh rate *)
let refresh = ref 0
let do_refresh () =
  !refresh mod !refresh_rate = 0 



(* graph drawing *)
let draw tortue canvas =
  match !root with
  | None -> ()
  | Some root -> 
    Ed_draw.draw_graph root tortue;
    Ed_display.draw_graph root canvas;
    if do_refresh () then
      canvas_root#canvas#update_now ()


let refresh_draw () =
  refresh := 0;    
  let tor = make_turtle !origine 0.0 in
  draw tor canvas_root

let refresh_display () =
  Ed_display.draw_graph !root canvas_root



let root_change vertex ()= 
  root := vertex; 
  origine := start_point;
  let turtle = make_turtle_origine () in
  draw turtle canvas_root




let node_selection ~(model : GTree.tree_store) path =
  let row = model#get_iter path in
  let vertex = model#get ~row ~column: Model.vertex in
  root_change (Some vertex) ()





(* usual function ref, for vertex event *)
let set_vertex_event_fun = ref (fun _ -> ())

(* type to select nature of modification *)
type modification = Add | Remove

(* add a vertex with no successor *)
let add_node () =
  let window = GWindow.window 
      ~title: "Choose vertex label" 
      ~width: 300 
      ~height: 50 
      ~position: `MOUSE
      () in
  let vbox = GPack.vbox 
      ~packing: window#add () in
  let entry = GEdit.entry 
      ~max_length: 50 
      ~packing: vbox#add () in
  entry#set_text "Label";
  entry#select_region 
    ~start:0 
    ~stop:entry#text_length;
  (*two check buttons allowing to add node to selection list and to choose this node as root*)
  let hbox = GPack.hbox 
      ~packing: vbox#add () in
  let is_in_selection = ref false in
  let in_selection = GButton.check_button  
      ~label: "Add to selection" 
      ~active:!is_in_selection
      ~packing: hbox#add () in
  ignore (in_selection#connect#toggled 
            ~callback:(fun () ->is_in_selection := in_selection#active ));
  let is_as_root = ref ((G.nb_vertex !graph)=0) in
  let as_root = GButton.check_button 
      ~label:"Choose as root" 
      ~active:!is_as_root 
      ~packing:hbox#add () in
  ignore (as_root#connect#toggled
            ~callback:(fun () ->is_as_root := as_root#active ));
  window#show ();
  (*entry's callback*)
  ignore( entry#connect#activate 
            ~callback: (fun () ->
                let text = entry#text in
                window#destroy ();
                (* new vertex *)
                let vertex = G.V.create (make_node_info text)  in
                G.add_vertex !graph  vertex ;
                ignore (Model.add_vertex vertex);
                Ed_display.add_node canvas_root vertex;
                !set_vertex_event_fun vertex;
                if !is_as_root  then root_change (Some vertex) () ;
                if !is_in_selection then update_vertex vertex Select;
                let  tor = make_turtle !origine 0.0 in
                draw tor canvas_root))






(* add an edge between n1 and n2 , add link in column and re-draw *)
let add_edge n1 n2 ()= 
  if not (edge n1 n2)
  then begin
    G.add_edge_e !graph (G.E.create n1 (make_edge_info ()) n2);
    Model.add_edge n1 n2;
    let tor = make_turtle !origine 0.0 in
    draw tor canvas_root;
  end


let add_edge_no_refresh n1 n2 ()= 
  if not (edge n1 n2)
  then begin
    G.add_edge_e !graph (G.E.create n1 (make_edge_info ()) n2);
    Model.add_edge n1 n2
  end

(* remove an edge between n1 and n2 , add un-link in column and re-draw *)
let remove_edge n1 n2 ()= 
  if (edge n1 n2)
  then begin
    G.remove_edge !graph  n1 n2;
    Model.remove_edge n1 n2;
    begin
      try
        let _,n = H2.find intern_edges (n1,n2) in
        n#destroy ();
        H2.remove intern_edges (n1,n2) 
      with Not_found -> ()
    end;
    begin
      try
        let _,n = H2.find intern_edges (n2,n1) in
        n#destroy ();
        H2.remove intern_edges (n2,n1) 
      with Not_found -> ()
    end;
    begin
      try
        let n = H2.find successor_edges (n1,n2) in
        n#destroy ();
        H2.remove successor_edges (n1,n2) 
      with Not_found -> ()
    end;
    begin
      try
        let n = H2.find successor_edges (n2,n1) in
        n#destroy ();
        H2.remove successor_edges (n2,n1) 
      with Not_found -> ()
    end;
    let tor = make_turtle !origine 0.0 in
    draw tor canvas_root;
  end


let remove_edge_no_refresh n1 n2 ()= 
  if (edge n1 n2)
  then begin
    G.remove_edge !graph n1 n2;
    Model.remove_edge n1 n2
  end



(* add successor node to selected node *)
let add_successor node () =
  let window = GWindow.window 
      ~title: "Choose label name" 
      ~width: 300 
      ~height: 50 
      ~position: `MOUSE
      () in
  let vbox = GPack.vbox 
      ~packing: window#add 
      () in
  let entry = GEdit.entry 
      ~max_length: 50 
      ~packing: vbox#add 
      () in
  entry#set_text "Label";
  entry#select_region 
    ~start:0 
    ~stop:entry#text_length;
  window#show ();
  ignore (entry#connect#activate 
            ~callback:(fun () ->
                let text = entry#text in
                window#destroy ();
                (* new vertex *)
                let vertex = G.V.create (make_node_info text)  in
                G.add_vertex !graph  vertex ;
                ignore (Model.add_vertex vertex);
                Ed_display.add_node canvas_root vertex;
                !set_vertex_event_fun vertex;
                (* new edge *)
                G.add_edge_e !graph (G.E.create node (make_edge_info()) vertex);
                Model.add_edge node vertex;
                (* redraw *)
                let tor = make_turtle !origine 0.0 in
                draw tor canvas_root
              )
         )

let remove_vertex vertex () =
  G.iter_succ
    (fun w ->
       begin 
         try
           let _,n = H2.find intern_edges (vertex,w) in
           n#destroy ();
           H2.remove intern_edges (vertex,w) 
         with Not_found -> ()
       end;
       begin
         try
           let _,n = H2.find intern_edges (w,vertex) in
           n#destroy ();      
           H2.remove intern_edges (w,vertex) 
         with Not_found -> ()
       end;
       begin       
         try
           let n = H2.find successor_edges (vertex,w) in
           n#destroy ();
           H2.remove successor_edges (vertex,w) 
         with Not_found -> ()
       end;
       begin       
         try
           let n = H2.find successor_edges (w,vertex) in
           n#destroy ();
           H2.remove successor_edges (w,vertex) 
         with Not_found -> ()
       end;
    )
    !graph vertex;
  let (n,_,_) =  H.find nodes vertex in
  n#destroy ();
  H.remove nodes vertex;
  ignore (Model.remove_vertex vertex);
  G.remove_vertex !graph vertex;
  begin  match !root with
    | None -> ()
    | Some root_v ->
      if (G.V.equal root_v vertex)
      then root := choose_root();
  end;
  refresh_draw ()



let sub_edge_to modif_type vertex list =
  let ll = List.length list in
  let nb_sub_menu = (ll - 1)/10 + 1 in
  let nb_edge =  ll / nb_sub_menu -1 in
  let menu = new GMenu.factory (GMenu.menu()) in
  let sub_menu =ref (new GMenu.factory (GMenu.menu())) in
  let add_menu_edge  vertex v2 =
    if not (G.V.equal v2 vertex)
    then begin
      match modif_type with
      | Add -> ignore((!sub_menu)#add_item (string_of_label v2) 
                        ~callback:( add_edge v2 vertex));
      | Remove -> ignore((!sub_menu)#add_item (string_of_label v2) 
                           ~callback:(remove_edge v2 vertex));
    end;
  in
  let rec make_sub_menu vertex list nb =
    match list with
    | [] -> ()
    | v::list ->
      match nb with
      | 0 -> 
        begin
          sub_menu :=new GMenu.factory (GMenu.menu()) ;
          add_menu_edge vertex v;
          let string = string_of_label v in
          ignore (menu#add_item (String.sub string 0 (min (String.length string) 3)^"...") 
                    ~submenu: !sub_menu#menu);
          make_sub_menu vertex list (nb+1);
        end
      | n when n= nb_edge-> 
        begin
          add_menu_edge vertex v;
          make_sub_menu vertex list 0
        end
      | _ ->
        begin
          add_menu_edge vertex v;
          make_sub_menu vertex list (nb+1)
        end
  in
  if ll > 10 
  then begin
    make_sub_menu vertex list 0;
    menu
  end
  else begin    
    let rec make_sub_bis list =
      match list with
      | [] -> ();
      | v::list ->add_menu_edge vertex v; make_sub_bis list
    in
    make_sub_bis list;
    !sub_menu
  end



let edge_to modif_type vertex list =
  (* add an edge between current vertex and one of selected vertex*)
  sub_edge_to modif_type vertex list



let all_edges (edge_menu :#GMenu.menu GMenu.factory) vertex list =
  (*add all edges as possible from current vertex to selected vertices*)
  begin
    let add_all_edge vertex list () = 
      List.iter (fun v -> if not (G.V.equal v vertex)
                  then add_edge_no_refresh v vertex()
                )
        list ;
      refresh := 0;    
      let tor = make_turtle !origine 0.0 in
      draw tor canvas_root
    in
    ignore (edge_menu#add_item "Add all edges" ~callback:( add_all_edge vertex list))
  end



let contextual_menu vertex ev =

  let menu = new GMenu.factory (GMenu.menu ()) in
  (* change root*)
  ignore (menu#add_item "As root" ~callback:(root_change (Some vertex)));

  (*vertex menu*)
  let vertex_menu = new GMenu.factory (GMenu.menu ()) in
  begin
    (* successor *)
    ignore (vertex_menu#add_item "Add successor" ~callback:(add_successor vertex));
    ignore (vertex_menu#add_separator ());
    (* remove vertex *)
    ignore(vertex_menu#add_item "Remove vertex" ~callback:(remove_vertex vertex));
  end;
  ignore(menu#add_item "Vertex ops" ~submenu: vertex_menu#menu);

  (*edge menu*)
  begin
    let add_list = selected_list (ADD_FROM vertex) in
    let rem_list = selected_list (REMOVE_FROM vertex) in
    let al =List.length add_list in    
    let rl =List.length rem_list in
    let isel = is_selected vertex in
    let menu_bool = ref false in
    let edge_menu = new GMenu.factory (GMenu.menu ()) in
    begin
      (* add menu *)
      if isel && al=2 
      || not isel && al=1
      then begin
        ignore (edge_menu#add_item "Add edge with" ~submenu: (edge_to Add vertex add_list)#menu);
        menu_bool := true;   
      end 
      else begin
        if isel && al>2 ||
           not isel && al>1
        then  begin
          ignore (edge_menu#add_item "Add edge with" ~submenu: (edge_to Add vertex add_list)#menu);
          all_edges edge_menu vertex add_list;
          menu_bool := true;   
        end
      end;
      (* remove menu *)
      if isel && rl>=2 ||
         not isel && rl>=1
      then begin
        if !menu_bool then ignore (edge_menu#add_separator ());
        ignore (edge_menu#add_item "Remove edge with" ~submenu: (edge_to Remove vertex rem_list)#menu);
        menu_bool := true;   
      end;
      if !menu_bool then ignore(menu#add_item "Edge ops" ~submenu: edge_menu#menu);
    end;
  end;    
  menu#menu#popup ~button:3 ~time:(GdkEvent.Button.time ev)



(* unit circle callback *)
let circle_event ev =
  begin match ev with
    | `BUTTON_PRESS ev ->
      if (GdkEvent.Button.button ev) = 3
      then
        begin
          let menu = new GMenu.factory (GMenu.menu ()) in
          ignore (menu#add_item " Add node" ~callback:(add_node));
          menu#menu#popup
            ~button:3
            ~time:(GdkEvent.Button.time ev)
        end
    | _ ->()
  end;
  true



(* event for each vertex of canvas *)
let vertex_event vertex item ellispe ev =

  (* let vertex_info = G.V.label vertex in*)
  begin match ev with
    | `ENTER_NOTIFY _ ->
      item#grab_focus ();
      update_vertex vertex Focus;
      refresh_display ()

    | `LEAVE_NOTIFY ev ->
      if not (Gdk.Convert.test_modifier `BUTTON1 (GdkEvent.Crossing.state ev))
      then begin
        update_vertex vertex Unfocus;
        refresh_display ()
      end

    | `BUTTON_RELEASE ev ->
      ellispe#parent#ungrab (GdkEvent.Button.time ev);

    | `MOTION_NOTIFY ev ->
      incr refresh;
      let state = GdkEvent.Motion.state ev in
      if Gdk.Convert.test_modifier `BUTTON1 state  then 
        begin
          let curs = Gdk.Cursor.create `FLEUR in
          ellispe#parent#grab [`POINTER_MOTION; `BUTTON_RELEASE] 
            curs (GdkEvent.Button.time ev);
          if do_refresh ()
          then begin
            let old_origin = !origine in
            let turtle = motion_turtle ellispe ev in
            if hspace_dist_sqr turtle <= rlimit_sqr then begin
              draw turtle canvas_root
            end else begin
              origine := old_origin;
              let turtle = { turtle with pos = old_origin } in
              draw turtle canvas_root
            end
          end
        end

    | `BUTTON_PRESS ev ->
      if (GdkEvent.Button.button ev) = 3
      then
        begin
          contextual_menu vertex ev
        end

    | `TWO_BUTTON_PRESS ev->
      if (GdkEvent.Button.button ev) = 1
      then begin
        if (Gdk.Convert.test_modifier `CONTROL (GdkEvent.Button.state ev))
        then begin
          if ( !nb_selected =0)
          then begin
            select_all ();
            update_vertex vertex Focus
          end
          else begin
            unselect_all ();
            update_vertex vertex Focus
          end
        end
        else begin
          if (is_selected vertex)
          then update_vertex vertex Unselect
          else update_vertex vertex Select;
        end;
        refresh_draw ();
      end;  

    | _ ->
      ()
  end;
  true


let set_vertex_event vertex =
  let item,ell,_ = H.find nodes vertex in
  ignore (item#connect#event ~callback:(vertex_event vertex item ell))

let () = set_vertex_event_fun := set_vertex_event

let set_canvas_event () =
  (* circle event *)
  ignore(canvas_root#parent#connect#event ~callback:(circle_event));
  (* vertex event *)
  G.iter_vertex set_vertex_event !graph


(* treeview *)
let add_columns ~(view : GTree.view) ~model =
  let renderer = GTree.cell_renderer_text [`XALIGN 0.] in
  let vc = GTree.view_column ~title:"Nodes" ~renderer:(renderer, ["text", Model.name]) ()
  in
  ignore (view#append_column vc);
  vc#set_sizing `FIXED;
  vc#set_fixed_width 100;
  (*  vc#set_resizable true;*)
  vc#set_sizing `AUTOSIZE;
  view#selection#connect#after#changed ~callback:
    begin fun () ->
      List.iter
        (fun p -> node_selection ~model p)
        view#selection#get_selected_rows;
    end


let _ = window#connect#destroy~callback:GMain.Main.quit 


let treeview = GTree.view ~model:Model.model ~packing:sw#add ()
let () = treeview#set_rules_hint true
let () = treeview#selection#set_mode `MULTIPLE
let _ = add_columns ~view:treeview ~model:Model.model





(* reset *)

let reset_table_and_canvas () =
  let l =  canvas_root#get_items in
  List.iter (fun v -> trace v#destroy ()) l;
  H2.clear intern_edges;
  H2.clear successor_edges;
  reset_display canvas_root;
  origine := start_point;
  nb_selected:=0



(* menu action functions *)

(*  choose a file to load or save to *)
let ask_for_file (mode: [< `OPEN | `SAVE]) =
  let default_file d = function
    | None -> d
    | Some v -> v
  in    
  let all_files () =
    let f = GFile.filter ~name:"All" () in
    f#add_pattern "*" ;
    f
  in
  let graph_filter () = 
    GFile.filter 
      ~name:"Fichier de graphes" 
      ~patterns:[ "*.dot"; "*.gml" ] ()
  in
  let dialog =
    begin match mode with
      | `OPEN ->  
        let dialog =
          GWindow.file_chooser_dialog 
            ~action: `OPEN
            ~title:"Open graph file"
            ~parent: window () in
        dialog#add_button_stock `CANCEL `CANCEL ;
        dialog#add_select_button_stock `OPEN `OPEN;
        dialog

      | `SAVE ->
        let dialog =
          GWindow.file_chooser_dialog 
            ~action: `SAVE
            ~title: "Save graph as..."
            ~parent: window 
            () in
        dialog#set_current_name "my_graph.dot";
        dialog#add_button_stock `CANCEL `CANCEL ;
        dialog#add_select_button_stock `SAVE `SAVE;
        dialog  
    end;
  in
  dialog#add_filter (graph_filter ()) ;
  dialog#add_filter (all_files ()) ;
  let f = match dialog#run () with
    | `OPEN -> default_file "<none>" dialog#filename 
    | `SAVE -> default_file "my_graph.dot" dialog#filename
    | `DELETE_EVENT | `CANCEL -> "<none>"
  in
  dialog#destroy ();
  f


(* menu action new graph *)      
let new_graph () =
  let alert_window = GWindow.message_dialog 
      ~message:("Are you sure you want to start"
                ^" a new graph and discard all"
                ^" unsaved changes to :\n\n"
                ^"<tt>\t"
                ^(match !graph_name with
                   | None -> "unamed"
                   | Some name -> name)
                ^"</tt>")
      ~use_markup:true
      ~title:"New graph ?"
      ~message_type:`QUESTION
      ~buttons:GWindow.Buttons.yes_no
      ~parent:window
      ~resizable:false
      ~position:`CENTER_ON_PARENT
      ()
  in
  begin 
    match alert_window#run () with
    | `YES  ->
      begin
        graph := G.create ();
        Model.reset();
        reset_table_and_canvas ();
        graph_name := None;
        set_window_title ()
      end 
    | `DELETE_EVENT | `NO -> ()
  end;
  alert_window#destroy ()



(* menu action open graph *)      
let open_graph ()  =
  let file = ask_for_file `OPEN  in
  if file <> "<none>"
  then 
    begin 
      load_graph file;
      reset_table_and_canvas ();
      let turtle = make_turtle_origine () in
      draw turtle canvas_root;
      set_canvas_event ()
    end

(* menu action save graph as *)      
let save_graph_as () =
  let file = ask_for_file `SAVE in
  if file <> "<none>"
  then begin 
    save_graph file;
    set_window_title ()
  end

(* menu action save graph *)
let save_graph () =
  match !graph_name with
  | None -> ()
  | Some name ->
    begin 
      save_graph name;
      set_window_title ()
    end

(* menu action quit *)      
let quit () =
  let alert_window = GWindow.message_dialog 
      ~message:("Are you sure you want to quit"
                ^"  and discard all"
                ^" unsaved changes to :\n\n"
                ^"<tt>\t"
                ^(match !graph_name with
                   | None -> "unamed"
                   | Some name -> name)
                ^"</tt>")
      ~use_markup:true
      ~title:"Quit ?"
      ~message_type:`QUESTION
      ~buttons:GWindow.Buttons.yes_no
      ~parent:window
      ~resizable:false
      ~position:`CENTER_ON_PARENT
      ()
  in
  begin 
    match alert_window#run () with
    | `YES  -> window#destroy ()
    | `DELETE_EVENT | `NO -> ()
  end    

(* menu action about *)      
let about () =
  let dialog = GWindow.about_dialog 
      ~authors:["Ocamlgraph :";
                "   Sylvain Conchon";
                "   Jean-Christophe Filliatre";
                "   Julien Signoles";
                "";
                ed_name^" :";
                "   Vadon Benjamin"]
      ~comments:" Ocamlgraph: a generic graph library for OCaml"
      ~copyright:"Copyright (C) 2004-2007 
Sylvain Conchon, Jean-Christophe Filliatre and Julien Signoles"
      ~license:" This software is free software; you can redistribute it and/or 
modify it under the terms of the GNU Library General Public 
License version 2, with the special exception on linking 
described in file LICENSE.                                         

This software is distributed in the hope that it will be useful, 
but WITHOUT ANY WARRANTY; without even the implied warranty of 
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE."
      ~logo:(GdkPixbuf.from_file"ed_icon.xpm" )
      ~name:ed_name
      ~version:"0.99"
      ~website:"http://ocamlgraph.lri.fr/"
      ~parent:window
      ~title:"About"
      ~resizable:false
      ~position:`CENTER_ON_PARENT
      ()
  in
  try ignore( dialog#run ())
  with Not_found -> dialog#destroy ()



let handbook_text (view:GText.view) =
  let buffer = view#buffer in
  (* text's tags *)
  ignore (buffer#create_tag ~name:"annotation" [`LEFT_MARGIN  10; `RIGHT_MARGIN 10;  `SIZE (7*Pango.scale)]);
  ignore (buffer#create_tag ~name:"center" [`JUSTIFICATION `CENTER]);
  ignore (buffer#create_tag ~name:"heading" [`UNDERLINE `SINGLE; `WEIGHT `BOLD; `SIZE (14*Pango.scale)]);
  ignore (buffer#create_tag ~name:"italic" [`LEFT_MARGIN  10; `RIGHT_MARGIN 10; `STYLE `ITALIC]);
  ignore (buffer#create_tag ~name:"item" [`LEFT_MARGIN  20; `RIGHT_MARGIN 10]);
  ignore (buffer#create_tag ~name:"subsection" [`LEFT_MARGIN  10; `RIGHT_MARGIN 10]);
  ignore (buffer#create_tag ~name:"title" [`WEIGHT `BOLD; `SIZE (17*Pango.scale);`JUSTIFICATION `CENTER]);
  ignore (buffer#create_tag ~name:"word_wrap" [`WRAP_MODE `WORD; `EDITABLE false]);
  let iter = buffer#get_iter_at_char 0 in
  (* title *)
  buffer#insert ~iter ~tag_names:["title"] (ed_name^" Handbook\n");
  (* editor's icon *)
  let image_anchor = buffer#create_child_anchor iter in
  let image = GMisc.image 
      ~pixbuf:(GdkPixbuf.from_file_at_size "ed_icon.xpm" ~width:70 ~height:70) 
      () in
  view#add_child_at_anchor image#coerce image_anchor;
  buffer#insert ~iter "\n\n\n";
  let start,stop = buffer#bounds in
  buffer#apply_tag_by_name "center" ~start ~stop ; 
  (* buffer's text *)
  buffer#insert ~iter ~tag_names:["heading"] "First words\n";
  buffer#insert ~iter ~tag_names:["subsection"] 
    ("\tFirst of all, you have to know this is an experimental application. " 
     ^"If you find a bug, please report it to the developers. "
     ^"This application has only basic functionalities on graphs, so if you want a new functionality, send it too.\n");
  buffer#insert ~iter ~tag_names:["subsection"] 
    (ed_name^" represents a graph in hyperbolic geometry, and specifically in Poincaré's disk representation.\n\n"
     ^ed_name^" is organized in four parts :\n");
  buffer#insert ~iter ~tag_names:["item"] "- a menu bar\n";
  buffer#insert ~iter ~tag_names:["item"] "- a vertex list, on the left side\n";
  buffer#insert ~iter ~tag_names:["item"] "- the Poincaré's disk\n";
  buffer#insert ~iter ~tag_names:["item"] "- and an associated contextual menu\n\n";
  buffer#insert ~iter ~tag_names:["heading"] "Menu bar\n";
  buffer#insert ~iter ~tag_names:["subsection"] 
    "\t It provides standard functionalities. You can create a new graph, open and save graphs from/to the Gml and Dot formats.\n"; 
  buffer#insert ~iter ~tag_names:["italic"] 
    "Don't forget to save your changes before create or load a new graph.\n\n";
  buffer#insert ~iter ~tag_names:["heading"] "Vertex list\n";
  buffer#insert ~iter ~tag_names:["subsection"] 
    "\t You can change the root the of graph diagram by clicking on a vertex name. If you expand one, you can see its descendants.\n\n"; 
  buffer#insert ~iter ~tag_names:["heading"] "Poincaré's disk\n";
  buffer#insert ~iter ~tag_names:["subsection"] 
    ("\t The graph is displayed in a disk. You can drag vertex to move the whole graph around.");
  buffer#insert ~iter ~tag_names:["annotation"] "[1]\n";
  buffer#insert ~iter ~tag_names:["subsection"] 
    ("By double-clicking on a node, you add/remove it to the current selection.\nWith a <Ctrl>+double-click you select all nodes, or unselect all (if one or more node is already selected)"
     ^"\n\n"); 
  buffer#insert ~iter ~tag_names:["heading"] "Contextual menu\n";
  buffer#insert ~iter ~tag_names:["subsection"] 
    ("\t This is the main way (and the only for the moment) to edit a graph. There are two different menus, but the distinction is transparent for the user.\n" 
     ^"The first is only composed by an 'adding node' menu, and appears when you click anywhere in the background of the disk.\n"
     ^"The second menu appears when you click on a vertex."
     ^" You can change root of the graph diagram, add a child node or remove the focused vertex."
     ^" The remainder of the menu depends on which nodes are selected. You can add or remove an edge with one of them selected, or with all."
     ^"\n\n"); 
  buffer#insert ~iter ~tag_names:["annotation"] "[1] :";
  buffer#insert ~iter ~tag_names:["subsection"] " a bug still remains, you can't drag root to much on the right-side, but on the left-side it is infinite";
  let start,stop = buffer#bounds in
  buffer#apply_tag_by_name "word_wrap" ~start ~stop ; 
  ()



(* menu action handbook *)
let handbook () =
  let dialog = GWindow.dialog
      ~width:450 
      ~height:450 
      ~title:"Handbook"
      () in 
  let view = GText.view () in
  let sw = GBin.scrolled_window ~packing:dialog#vbox#add ()
  in
  sw#add view#coerce;
  handbook_text view;
  dialog#add_button_stock `CLOSE `CLOSE;
  match dialog#run () with
  | `CLOSE | `DELETE_EVENT -> dialog#destroy ()

(* menu bar, based on ui_manager *)
let ui_info = "<ui>\
               <menubar name='MenuBar'>\
               <menu action='FileMenu'>\
               <menuitem action='New graph'/>\
               <menuitem action='Open graph'/>\
               <menuitem action='Save graph'/>\
               <menuitem action='Save graph as...'/>\
               <separator/>\
               <menuitem action='Quit'/>\
               </menu>\
               <menu action='HelpMenu'>\
               <menuitem action='About'/>\
               <menuitem action='Handbook'/>\
               </menu>\
               </menubar>\
               </ui>"

(* choose right menu action *)
let activ_action ac =
  let name = ac#name in
  match name with
  | "New graph" -> new_graph ()
  | "Open graph"-> open_graph ()
  | "Save graph" -> save_graph ()
  | "Save graph as..." -> save_graph_as ()
  | "Quit" -> quit ()
  | "About" -> about ()
  | "Handbook" -> handbook ()
  | _ -> Format.eprintf "%s menu is not yet implemented @." name


let setup_ui window = 
  let add_action = GAction.add_action in
  let actions = GAction.action_group ~name:"Actions" () in
  GAction.add_actions actions
    [ add_action "FileMenu" ~label:"_File" ;
      add_action "HelpMenu" ~label:"_Help" ;

      add_action "New graph" ~stock:`NEW ~tooltip:"Create a new graph"
        ~callback:activ_action ;
      add_action "Open graph" ~stock:`OPEN ~tooltip:"Open a graph file"
        ~callback:activ_action ;
      add_action "Save graph" ~stock:`SAVE ~tooltip:"Save current graph"
        ~callback:activ_action ;
      add_action "Save graph as..." ~stock:`SAVE_AS ~accel:"<Control><Shift>S" 
        ~tooltip:"Save current graph to specified file" 
        ~callback:activ_action ;
      add_action "Quit" ~stock:`QUIT ~tooltip:"Quit"
        ~callback:activ_action ;
      add_action "About" ~label:"_About" ~tooltip:"Who build this"
        ~callback:activ_action;
      add_action "Handbook" ~label:"_Handbook" ~accel:"<Control>H" ~tooltip:"How to.."
        ~callback:activ_action;
    ] ;
  let ui_m = GAction.ui_manager () in
  ui_m#insert_action_group actions 0 ;
  window#add_accel_group ui_m#get_accel_group ;
  ignore (ui_m#add_ui_from_string ui_info) ;
  menu_bar_box#pack (ui_m#get_widget "/MenuBar") 



let () = 
  reset_table_and_canvas ();
  draw (make_turtle_origine ()) canvas_root;
  set_canvas_event ();
  canvas#set_scroll_region ~x1:0. ~y1:0. ~x2:w ~y2:h ;
  setup_ui window;
  ignore (window#show ());
  GMain.Main.main ()

