
(** TODO: binding -- document.getElementById -- to mount node *)

type html_element 

class type doc = 
  object
    method getElementById : string -> html_element
  end
external doc :  doc = "doc" [@@js.val ]

class type console = 
    object 
        method log : 'a -> unit
    end
external console : console  = "console" [@@js.val ]

let v = console#log "hey";;
let u = console
let v = doc#getElementById "haha"
external log : 'a -> unit = "" [@@js.call "console.log"]
let v = log 32
type t 
type element
external document :  t = "document" [@@js.val ] [@@js.scope "window"]
external getElementById : t -> string -> element = "getElementById" [@@js.send ]        


type config 
type component
external config :
      ?display_name:string ->
        render:(unit -> component) -> unit -> 
          config = "" [@@js.obj ]

type attrs
external attrs:
        ?alt: string -> 
        ?autoPlay: bool -> 
          unit -> attrs = "" [@@js.obj]


external str : string -> component = "%identity"            
external h1 : ?attrs:attrs -> component array  -> component = "" 
    [@@js.call "h1"] [@@js.scope "DOM"] [@@js.module"react"] [@@js.splice]

external h2 : ?attrs:attrs -> component array  -> component = ""
    [@@js.call "h2"] [@@js.scope  "DOM"][@@js.module "react"] [@@js.splice]
external h3 : ?attrs:attrs -> component array  -> component = ""
    [@@js.call "h3"] [@@js.scope  "DOM"][@@js.module "@" "react"] [@@js.splice]

external h4 : ?attrs:attrs -> component array  -> component = ""
    [@@js.call "h4"] [@@js.scope  "DOM"][@@js.module "@" "react"] [@@js.splice]

external div : ?attrs:attrs -> component array ->  component = ""
    [@@js.call "div"] [@@js.scope "DOM"][@@js.module "react"] [@@js.splice]

type component_class
external createClass : 
      config -> component_class = "createClass" 
        [@@js.call "createClass"]
        [@@js.module "react"]

external render : component_class -> element -> unit = "" 
    [@@js.call "render"]
    [@@js.module "react-dom"] (* TODO: error checking -- attributes todo*)
;;

render (
     createClass (
     (config
       ~render:(fun _ -> 
         div 
              ~attrs:(attrs ~alt:"pic" ())
              [|
                h1 [| str "hello react"|];
                h2 [| str "type safe!" |];
                h3 [| str "type safe!" |];
              |]
               )
        ()))) (getElementById document  "hi")
  
    

;;

let u = 33
