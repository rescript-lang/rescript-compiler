
(** TODO: binding -- document.getElementById -- to mount node *)

type html_element 

class type doc = 
  object
    method getElementById : string -> html_element
  end
external doc :  doc = "doc" [@@bs.val ]

class type console = 
    object 
        method log : 'a -> unit
    end
external console : console  = "console" [@@bs.val ]

let v = console#log "hey";;
let u = console
let v = doc#getElementById "haha"
external log : 'a -> unit = "" [@@bs.call "console.log"]
let v = log 32
type t 
type element
external document :  t = "document" [@@bs.val ] [@@bs.scope "window"]
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
        ?autoPlay: bool -> 
          unit -> attrs = "" [@@bs.obj]


external str : string -> component = "%identity"            
external h1 : ?attrs:attrs -> component array  -> component = "" 
    [@@bs.call "h1"] [@@bs.scope "DOM"] [@@bs.module"react"] [@@bs.splice]

external h2 : ?attrs:attrs -> component array  -> component = ""
    [@@bs.call "h2"] [@@bs.scope  "DOM"][@@bs.module "react"] [@@bs.splice]
external h3 : ?attrs:attrs -> component array  -> component = ""
    [@@bs.call "h3"] [@@bs.scope  "DOM"][@@bs.module "@" "react"] [@@bs.splice]

external h4 : ?attrs:attrs -> component array  -> component = ""
    [@@bs.call "h4"] [@@bs.scope  "DOM"][@@bs.module "@" "react"] [@@bs.splice]

external div : ?attrs:attrs -> component array ->  component = ""
    [@@bs.call "div"] [@@bs.scope "DOM"][@@bs.module "react"] [@@bs.splice]

type component_class
external createClass : 
      config -> component_class = "createClass" 
        [@@bs.call "createClass"]
        [@@bs.module "react"]

external render : component_class -> element -> unit = "" 
    [@@bs.call "render"]
    [@@bs.module "react-dom"] (* TODO: error checking -- attributes todo*)
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
