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
    method title__w : string -> unit 
    method title__ : string
  end

class type text = 
    object
      method text__w : string -> unit 
      method text__ : string 
    end
class type measure =
    object
      method minHeight__w : int -> unit 
      method minHeight__r : int
      method minWidth__w : int -> unit 
      method minWidth__r : int 
      method maxHeight__w : int -> unit 
      method maxHeight__r : int
      method maxWidth__w : int -> unit 
      method maxWidth__r : int 

    end

class type layout = 
    object 
      method orientation__w : string -> unit 
      method orientation__r : string
    end

class type applicationContext = 
  object 
      method exit : int -> unit 
          (* exit'overloading : int -> string -> unit *)
  end
class type contentable = 
  object
    method content__w : #widget -> unit 
    method content__r : #widget
    method contentWidth__w : int -> unit 
    method contentWidth__r : int -> unit 
  end

class type hostedWindow =
  object 
    inherit widget 
    inherit title
    inherit contentable
    method show : unit -> unit 
    method hide : unit -> unit
    method focus : unit -> unit 
    method appContext__w : applicationContext -> unit 
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
    method addChild__1 : #widget -> unit 
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




external mk_text : text: 'b -> <text : 'b>  = "" [@@bs.obj]
external mk_label : label : 'a -> <label: 'a > = "" [@@bs.obj]
external mk_width : width : 'a -> <width: 'a> = "" [@@bs.obj]
external mk_column : width: int -> unit -> column = "" [@@bs.obj]
external mk_titleRow : title: string -> unit ->  titleRow = "" [@@bs.obj]

class type grid  = 
  object
    inherit widget
    inherit measure
    method columns__w : <width : int; .. >  array -> unit 
    method titleRows__w : <label : <text : string; .. > ; ..>  array -> unit 
    method dataSource__w : <label : <text : string; .. > ; ..> array array -> unit  
  end

external set_interval : (unit -> unit) -> float -> unit  = "" 
    [@@bs.call "setInterval"] [@@bs.module "@runtime" "Runtime"]
external set_grid_columns : grid -> column array -> unit = ""  [@@bs.call "set"]
external set_grid_titleRows : grid -> string array -> unit = "" [@@bs.call "set"]
external to_fixed : float -> int -> string = ""[@@bs.send "toFixed"]

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
external addChild : stackPanel -> #widget -> unit = "x" [@@bs.send]


external new_HostedWindow : unit -> hostedWindow = "" 
    [@@bs.new "HostedWindow"] [@@bs.module "@blp/ui" "BUI"]

external new_HostedContent : unit -> hostedContent = "" 
    [@@bs.new "HostedContent"] [@@bs.module "@blp/ui" "BUI"]

external new_StackPanel : unit -> stackPanel = "" 
    [@@bs.new "StackPanel"] [@@bs.module "@ui" "UI"]

external new_textArea : unit -> textArea = "" 
    [@@bs.new "TextArea"] [@@bs.module "@ui" "UI"]

external new_button : unit -> button = ""
    [@@bs.new "Button"] [@@bs.module "@ui" "UI"]

external new_grid : unit -> grid = ""
    [@@bs.new "Grid"] [@@bs.module "@ui" "UI"]

(* Note, strictly speaking, it 's not returning a primitive string, it returns
   an object string *)
external stringify : 'a -> string = ""
    [@@bs.new "String"] 

external random : unit -> float = ""
    [@@bs.call "Math.random"] 
external array_map : 'a array -> ('a -> 'b) -> 'b array = "" [@@bs.call"Array.prototype.map.call"] 
type env 
external mk_bid_ask : bid:float -> ask:float -> env = "" [@@bs.obj]  


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
    hw1#appContext__w appContext;
    hw1#title__w "Test Application From OCaml";
    hw1#content__w hc;


    hc#contentWidth__w 700;
    hc#content__w stackPanel;

    stackPanel#orientation__w "vertical";
    stackPanel#minHeight__w 10000; (* FIXME -> 1e4 *)
    stackPanel#minWidth__w 4000;

    stackPanel#addChild__1 grid;
    stackPanel#addChild inputCode;
    stackPanel#addChild button;

    let mk_titleRow = fun text -> (mk_label ~label:(mk_text ~text )) in
    let u = mk_width 200 in
    grid#minHeight__w 300;
    grid#titleRows__w 
        [| mk_titleRow "Ticker";
           mk_titleRow "Bid";
           mk_titleRow "Ask";
           mk_titleRow "Result" |] ;
    grid#columns__w [| u;u;u;u |];

    inputCode#text__w " bid - ask";
    inputCode#minHeight__w 100;

    button#text__w "update formula";
    button#minHeight__w 20;
    button # on "click" (fun _ -> 
      try 
        let hot_function = compile inputCode#text__ in
        computeFunction := fun env ->  hot_function (fun key -> lookup env key) 
      with  e -> ());
    let fmt v = to_fixed v 2 in
    set_interval (fun _ -> 

      grid#dataSource__w 
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

