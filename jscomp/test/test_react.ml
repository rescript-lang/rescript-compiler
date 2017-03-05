
(** TODO: binding -- document.getElementById -- to mount node *)

type html_element 

class type document = 
  object
    method getElementById : string -> html_element 
  end[@bs]

type doc = document Js.t 
external doc :  doc  = "doc" [@@bs.val ]

class type con = 
  object
    method log : 'a -> unit 
  end[@bs]

type console = con Js.t 
external console : console  = "console" [@@bs.val ]

let v = console##log "hey";;
let u = console
let v = doc##getElementById "haha"

external log : 'a -> unit = "" [@@bs.val "console.log"]
let v = log 32
type t 
type element
external document :  t = "document" [@@bs.val ] 
external getElementById : t -> string -> element = "getElementById" [@@bs.send ]        


type config 
type component
external config :
      ?display_name:string ->
        render:(unit -> component) -> unit -> 
          config = "" [@@bs.obj ]

type attrs
external attrs:
        ?alt: string -> 
        ?autoPlay: Js.boolean -> 
          unit -> attrs = "" [@@bs.obj]


external str : string -> component = "%identity"            
type vdom 
external vdom : vdom = "DOM" [@@bs.module "react"] [@@bs.val]


(* FIXME: investigate 
   cases:
   {[
     [@@bs.module "package1" "same_name"]
     [@@bs.module "package2" "same_name"]
   ]}
   {[
     [@@bs.module "package" "name1"]
     [@@bs.module "package" "name2"]
   ]}
*)
external h1 : vdom -> ?attrs:attrs -> component array  -> component = "h1" 
    [@@bs.send]  [@@bs.splice]
external h2 : vdom -> ?attrs:attrs -> component array  -> component = "h2" 
    [@@bs.send]  [@@bs.splice]

external h3 : vdom ->  ?attrs:attrs -> component array  -> component = "h3"
    [@@bs.send]  [@@bs.splice]

external h4 : vdom ->  ?attrs:attrs -> component array  -> component = "h4"
    [@@bs.send]  [@@bs.splice]

external div : vdom -> ?attrs:attrs -> component array ->  component = "div"
    [@@bs.send]  [@@bs.splice]

type component_class
external createClass : 
      config -> component_class = "createClass" 
        [@@bs.val "createClass"]
        [@@bs.module "react"]

external render : component_class -> element -> unit = "" 
    [@@bs.val "render"]
    [@@bs.module "react-dom"] (* TODO: error checking -- attributes todo*)
;;

render (
     createClass (
     (config
       ~render:(fun _ -> 
         div vdom
              ~attrs:(attrs ~alt:"pic" ())
              [|
                h1 vdom [| str "hello react"|];
                h2 vdom [| str "type safe!" |];
                h3 vdom [| str "type safe!" |];
              |]
               )
        ()))) (getElementById document  "hi")
  
    

;;

let u = 33
