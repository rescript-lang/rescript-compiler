open Demo_binding

external addChild : stackPanel -> #widget -> unit = "x" [@@bs.send]

external new_HostedWindow : unit -> hostedWindow Js.t = "HostedWindow"
  [@@bs.new] [@@bs.module "@blp/ui", "BUI"]

external new_HostedContent : unit -> hostedContent Js.t = ""
  [@@bs.new "HostedContent"] [@@bs.module "@blp/ui", "BUI"]

external new_StackPanel : unit -> stackPanel Js.t = ""
  [@@bs.new "StackPanel"] [@@bs.module "@ui", "UI"]

external new_textArea : unit -> textArea Js.t = ""
  [@@bs.new "TextArea"] [@@bs.module "@ui", "UI"]

external new_button : unit -> button Js.t = ""
  [@@bs.new "Button"] [@@bs.module "@ui", "UI"]

external new_grid : unit -> grid Js.t = ""
  [@@bs.new "Grid"] [@@bs.module "@ui", "UI"]

(* Note, strictly speaking, it 's not returning a primitive string, it returns
   an object string *)
external stringify : 'a -> string = "" [@@bs.new "String"]
external random : unit -> float = "" [@@bs.val "Math.random"]

external array_map : 'a array -> (('a -> 'b)[@bs]) -> 'b array = ""
  [@@bs.val "Array.prototype.map.call"]

type env

external mk_bid_ask : bid:float -> ask:float -> env = "" [@@bs.obj]

type data = {ticker: string; price: float}

let data =
  [| {ticker= "GOOG"; price= 700.0}
   ; {ticker= "AAPL"; price= 500.0}
   ; {ticker= "MSFT"; price= 300.0} |]

let ui_layout (compile : string -> (string -> float) -> float) lookup
    appContext : hostedWindow Js.t =
  let init = compile "bid  - ask" in
  let computeFunction = ref (fun env -> init (fun key -> lookup env key)) in
  let hw1 = new_HostedWindow () in
  let hc = new_HostedContent () in
  let stackPanel = new_StackPanel () in
  let inputCode = new_textArea () in
  let button = new_button () in
  let grid = new_grid () in
  hw1##appContext#=appContext ;
  hw1 ## title #= "Test Application From OCaml" ;
  hw1##content#=hc ;
  hc ## contentWidth #= 700 ;
  hc##content#=stackPanel ;
  stackPanel ## orientation #= "vertical" ;
  stackPanel ## minHeight #= 10000 ;
  (* FIXME -> 1e4 *)
  stackPanel ## minWidth #= 4000 ;
  stackPanel##addChild grid ;
  stackPanel##addChild inputCode ;
  stackPanel##addChild button ;
  let mk_titleRow text = [%bs.obj {label= {text}}] in
  let u = [%bs.obj {width= 200}] in
  grid ## minHeight #= 300 ;
  grid ## titleRows
  #= [| mk_titleRow "Ticker"; mk_titleRow "Bid"; mk_titleRow "Ask"
      ; mk_titleRow "Result" |] ;
  grid ## columns #= [|u; u; u; u|] ;
  inputCode ## text #= " bid - ask" ;
  inputCode ## minHeight #= 100 ;
  button ## text #= "update formula" ;
  button ## minHeight #= 20 ;
  button##on "click" (fun [@bs] _event ->
      (* FIXME both [_] and () should work*)
      try
        let hot_function = compile inputCode##text in
        computeFunction := fun env -> hot_function (fun key -> lookup env key)
      with e -> ()) ;
  let fmt v = toFixed v 2 in
  set_interval
    (fun [@bs] () ->
      grid ## dataSource
      #= (array_map data (fun [@bs] {ticker; price} ->
              let bid = price +. (20. *. random ()) in
              let ask = price +. (20. *. random ()) in
              let result = !computeFunction (mk_bid_ask ~bid ~ask) in
              [| mk_titleRow ticker
               ; mk_titleRow (fmt bid)
               ; mk_titleRow (fmt ask)
               ; mk_titleRow (fmt result) |])))
    100. ;
  hw1
