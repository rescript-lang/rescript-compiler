(* open Ui_defs  *)

class type widget = 
  object 
      method on : string * (event -> unit [@uncurry]) -> unit [@uncurry]
  end
and  event = 
  object 
    method source : widget
    method target : widget
  end


class type title = 
  object
    method title__set : string -> unit [@uncurry]
    method title : string
  end

class type text = 
    object
      method text__set : string -> unit [@uncurry]
      method text : string 
    end
class type measure =
    object
      method minHeight__set : int -> unit [@uncurry]
      method minHeight : int
      method minWidth__set : int -> unit [@uncurry]
      method minWidth : int 
      method maxHeight__set : int -> unit [@uncurry]
      method maxHeight : int [@uncurry]
      method maxWidth__set : int -> unit [@uncurry]
      method maxWidth : int 

    end

class type layout = 
    object 
      method orientation__set : string -> unit  [@uncurry]
      method orientation : string
    end

class type applicationContext = 
  object 
      method exit : int -> unit [@uncurry]
          (* exit'overloading : int -> string -> unit *)
  end
class type contentable = 
  object
    method content__set : #widget Js.t -> unit [@uncurry]
    method content : #widget Js.t [@uncurry]
    method contentWidth : int  
    method contentWidth__set : int -> unit [@uncurry]
  end

class type hostedWindow =
  object 
    inherit widget 
    inherit title
    inherit contentable
    method show : unit -> unit [@uncurry]
    method hide : unit -> unit [@uncurry]
    method focus : unit -> unit [@uncurry]
    method appContext__set : applicationContext -> unit [@uncurry]
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

    method addChild : #widget Js.t -> unit [@uncurry]

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
    method columns__set : <width : int; .. >  array -> unit [@uncurry]
    method titleRows__set : 
      <label : <text : string; .. > ; ..>  array -> unit [@uncurry]
    method dataSource__set :
      <label : <text : string; .. > ; ..> array array -> unit  [@uncurry]
  end

external set_interval : (unit -> unit [@uncurry]) -> float -> unit  = "" 
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


external new_HostedWindow : unit -> hostedWindow Js.t = "HostedWindow"
    [@@bs.new ] [@@bs.module "@blp/ui" "BUI"]

external new_HostedContent : unit -> hostedContent Js.t = "" 
    [@@bs.new "HostedContent"] [@@bs.module "@blp/ui" "BUI"]

external new_StackPanel : unit -> stackPanel Js.t = "" 
    [@@bs.new "StackPanel"] [@@bs.module "@ui" "UI"]

external new_textArea : unit -> textArea Js.t = "" 
    [@@bs.new "TextArea"] [@@bs.module "@ui" "UI"]

external new_button : unit -> button Js.t = ""
    [@@bs.new "Button"] [@@bs.module "@ui" "UI"]

external new_grid : unit -> grid Js.t = ""
    [@@bs.new "Grid"] [@@bs.module "@ui" "UI"]

(* Note, strictly speaking, it 's not returning a primitive string, it returns
   an object string *)
external stringify : 'a -> string = ""
    [@@bs.new "String"] 

external random : unit -> float = ""
    [@@bs.call "Math.random"] 

external array_map : 'a array -> ('a -> 'b [@uncurry]) -> 'b array = ""
    [@@bs.call"Array.prototype.map.call"] 

type env 
external mk_bid_ask : bid:float -> ask:float -> env = "" [@@bs.obj]  


type data = { ticker : string ; price : float }


let data = 
[|
  { ticker = "GOOG" ; price = 700.0; };
  { ticker = "AAPL" ; price = 500.0; };
  { ticker = "MSFT" ; price = 300.0; }
|];;


let ui_layout 
    (compile  : string -> (string -> float) -> float) lookup  appContext
  : hostedWindow Js.t = 
  let init = compile "bid  - ask" in
  let computeFunction = ref (fun env -> init (fun key -> lookup env key) ) in
  let hw1 = new_HostedWindow ()  in
  let hc = new_HostedContent () in
  let stackPanel = new_StackPanel () in
  let inputCode = new_textArea () in

  let button = new_button () in
  let grid = new_grid () in
  begin 
    hw1##appContext__set appContext;
    hw1##title__set "Test Application From OCaml";
    hw1##content__set hc;


    hc##contentWidth__set 700;
    hc##content__set stackPanel;

    stackPanel##orientation__set "vertical";
    stackPanel##minHeight__set 10000; (* FIXME -> 1e4 *)
    stackPanel##minWidth__set 4000;

    stackPanel##addChild grid;
    stackPanel##addChild inputCode;
    stackPanel##addChild button;

    let mk_titleRow = fun text -> (mk_label ~label:(mk_text ~text )) in
    let u = mk_width 200 in
    grid##minHeight__set 300;
    grid##titleRows__set
        [| mk_titleRow "Ticker";
           mk_titleRow "Bid";
           mk_titleRow "Ask";
           mk_titleRow "Result" |] ;
    grid##columns__set [| u;u;u;u |];

    inputCode##text__set " bid - ask";
    inputCode##minHeight__set 100;

    button##text__set "update formula";
    button##minHeight__set 20;
    button##on ("click", (fun %uncurry _event -> (* FIXME both [_] and () should work*)
      try 
        let hot_function = compile inputCode#.text in
        computeFunction := fun env ->  hot_function (fun key -> lookup env key) 
      with  e -> ()));
    let fmt v = to_fixed v 2 in
    set_interval (fun %uncurry () -> 

      grid##dataSource__set
        ( array_map data (fun %uncurry {ticker; price } -> 
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

