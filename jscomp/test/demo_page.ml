let rec fib = function
  | 1 | 2 -> 1
  | n -> fib (n - 1 )  + fib (n - 2)
(** Imperative style *)
let sum n =
    let v  = ref 0 in
    for i = 0 to n do
       v := !v + i
    done;
    !v
(** List map *)
type 'a list =
  | Nil
  | Cons of 'a * 'a list

let rec map f = function
  | Nil -> Nil
  | Cons (x,xs) ->  Cons (f x, map f xs)

(** Test curry and uncurry calling convention *)
let test_curry x  y =  x + y
let f = test_curry 32

(** Create a typed binding for react *)
type t
type element
external document : unit -> t = "" [@@js.global "document"] [@@js.scope "window"]
external getElementById : t -> string -> element = "" [@@js.send "getElementById"]

(** Phantom types *)
type config
type component
type attrs
type component_class

external config :
      ?display_name:string ->
        render:(unit -> component) -> unit ->
          config = "" [@@js.obj ] (** make a json object *)
external attrs:
        ?alt: string ->
        ?autoPlay: bool ->
          unit -> attrs = "" [@@js.obj]
external str : string -> component = "%identity"
external h1 : ?attrs:attrs -> component array  -> component = ""
    [@@js.call "h1"] [@@js.scope  "DOM"] [@@js.module "react"] [@@js.splice] (** splice the last argument *)
external h2 : ?attrs:attrs -> component array  -> component = ""
    [@@js.call "h2"] [@@js.scope  "DOM"] [@@js.module "react"] [@@js.splice]
external div : ?attrs:attrs -> component array ->  component = ""
    [@@js.call "div"] [@@js.scope "DOM"] [@@js.module "react"]  [@@js.splice]
external createClass :
      config -> component_class = "createClass"
        [@@js.call "createClass"]
        [@@js.module "react"]
external render : component_class -> element -> unit = ""
    [@@js.call "render"]
    [@@js.module "react-dom"]
;;
(** Do the rendering *)
render (
     createClass (
     (config
       ~render:(fun _ ->
         div
              ~attrs:(attrs ~alt:"pic" ())
              [|
                h1 [| str "hello react"|];
                h2 [| str "type safe!" |];
  |]
               )
        ()))) (getElementById (document ()) "hi")
