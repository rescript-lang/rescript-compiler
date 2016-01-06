(* open Ui_defs  *)

class type widget = 
  object 
      method on : string -> (event -> unit) -> unit 
  end
and  event = 
  object 
    method source : widget
    method target : widget
  end


class type title = 
  object
    method _set_title : string -> unit 
    method _get_title : string
  end

class type text = 
    object
      method _set_text : string -> unit 
      method _get_text : string 
    end
class type measure =
    object
      method _set_minHeight : int -> unit 
      method _get_minHeight : int
      method _set_minWidth : int -> unit 
      method _get_minWidth : int 
      method _set_maxHeight : int -> unit 
      method _get_maxHeight : int
      method _set_maxWidth : int -> unit 
      method _get_maxWidth : int 

    end

class type layout = 
    object 
      method _set_orientation : string -> unit 
      method _get_orientation : string
    end

class type applicationContext = 
  object 
      method exit : int -> unit 
          (* exit'overloading : int -> string -> unit *)
  end
class type contentable = 
  object
    method _set_content : #widget -> unit 
    method _get_content : #widget
    method _set_contentWidth : int -> unit 
    method _get_contentWidth : int -> unit 
  end

class type hostedWindow =
  object 
    inherit widget 
    inherit title
    inherit contentable
    method show : unit -> unit 
    method hide : unit -> unit
    method focus : unit -> unit 
    method _set_appContext : applicationContext -> unit 
  end

class type hostedContent =
  object 
    inherit widget
    inherit contentable
  end


class type stackPanel = 
  object 
    inherit measure
    inherit layout 
    inherit widget
    method addChild : #widget -> unit 
  end

(* class type columns =  *)
(*   object  *)
(*       method width : int    *)
(*   end *)
class type any = 
  object 
  end

type column 
type titleRow 




external mk_text : text: 'b -> <text : 'b>  = "" [@@js.obj]
external mk_label : label : 'a -> <label: 'a > = "" [@@js.obj]
external mk_width : width : 'a -> <width: 'a> = "" [@@js.obj]
external mk_column : width: int -> unit -> column = "" [@@js.obj]
external mk_titleRow : title: string -> unit ->  titleRow = "" [@@js.obj]

class type grid  = 
  object
    inherit widget
    inherit measure
    method _set_columns : <width : int; .. >  array -> unit 
    method _set_titleRows : <label : <text : string; .. > ; ..>  array -> unit 
    method _set_dataSource : <label : <text : string; .. > ; ..> array array -> unit  
  end

external set_interval : (unit -> unit) -> float -> unit  = "" 
    [@@js.call "setInterval"] [@@js.module "@runtime" "Runtime"]
external set_grid_columns : grid -> column array -> unit = ""  [@@js.call "set"]
external set_grid_titleRows : grid -> string array -> unit = "" [@@js.call "set"]
external to_fixed : float -> int -> string = ""[@@js.send "toFixed"]

class type button = 
  object
    inherit widget
    inherit text
    inherit measure
  end

class type textArea = 
    object
      inherit widget
      inherit measure
      inherit text 
    end
external addChild : stackPanel -> #widget -> unit = "x" [@@js.send]


external new_HostedWindow : unit -> hostedWindow = "" 
    [@@js.new "HostedWindow"] [@@js.module "@blp/ui" "BUI"]

external new_HostedContent : unit -> hostedContent = "" 
    [@@js.new "HostedContent"] [@@js.module "@blp/ui" "BUI"]

external new_StackPanel : unit -> stackPanel = "" 
    [@@js.new "StackPanel"] [@@js.module "@ui" "UI"]

external new_textArea : unit -> textArea = "" 
    [@@js.new "TextArea"] [@@js.module "@ui" "UI"]

external new_button : unit -> button = ""
    [@@js.new "Button"] [@@js.module "@ui" "UI"]

external new_grid : unit -> grid = ""
    [@@js.new "Grid"] [@@js.module "@ui" "UI"]

(* Note, strictly speaking, it 's not returning a primitive string, it returns
   an object string *)
external stringify : 'a -> string = ""
    [@@js.new "String"] 

external random : unit -> float = ""
    [@@js.call "Math.random"] 
external array_map : 'a array -> ('a -> 'b) -> 'b array = "" [@@js.call"Array.prototype.map.call"] 
type env 
external mk_bid_ask : bid:float -> ask:float -> env = "" [@@js.obj]  


type data = { ticker : string ; price : float }


let data = 
[|
  { ticker = "GOOG" ; price = 700.0; };
  { ticker = "AAPL" ; price = 500.0; };
  { ticker = "MSFT" ; price = 300.0; }
|];;


let ui_layout (compile  : string -> (string -> float) -> float) lookup  appContext : hostedWindow = 
  let init = compile "bid  - ask" in
  let computeFunction = ref (fun env -> init (fun key -> lookup env key) ) in
  let hw1 = new_HostedWindow ()  in
  let hc = new_HostedContent () in
  let stackPanel = new_StackPanel () in
  let inputCode = new_textArea () in

  let button = new_button () in
  let grid = new_grid () in
  begin 
    hw1#_set_appContext appContext;
    hw1#_set_title "Test Application From OCaml";
    hw1#_set_content hc;


    hc#_set_contentWidth 700;
    hc#_set_content stackPanel;

    stackPanel#_set_orientation "vertical";
    stackPanel#_set_minHeight 10000; (* FIXME -> 1e4 *)
    stackPanel#_set_minWidth 4000;

    stackPanel#addChild grid;
    stackPanel#addChild inputCode;
    stackPanel#addChild button;

    let mk_titleRow = fun text -> (mk_label ~label:(mk_text ~text )) in
    let u = mk_width 200 in
    grid#_set_minHeight 300;
    grid#_set_titleRows 
        [| mk_titleRow "Ticker";
           mk_titleRow "Bid";
           mk_titleRow "Ask";
           mk_titleRow "Result" |] ;
    grid#_set_columns [| u;u;u;u |];

    inputCode#_set_text " bid - ask";
    inputCode#_set_minHeight 100;

    button#_set_text "update formula";
    button#_set_minHeight 20;
    button # on "click" (fun _ -> 
      try 
        let hot_function = compile inputCode#_get_text in
        computeFunction := fun env ->  hot_function (fun key -> lookup env key) 
      with  e -> ());
    let fmt v = to_fixed v 2 in
    set_interval (fun _ -> 

      grid#_set_dataSource 
        ( array_map data (function {ticker; price } -> 
          let bid = price +. 20. *. random () in
          let ask = price +. 20. *. random () in
          let result = !computeFunction (mk_bid_ask ~bid ~ask ) in
          [| mk_titleRow ticker;
             mk_titleRow (fmt bid);
             mk_titleRow (fmt ask);
             mk_titleRow (fmt result)
           |]))) 100. ; 
    hw1
  end

