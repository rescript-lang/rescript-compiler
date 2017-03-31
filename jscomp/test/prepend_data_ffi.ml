
type config1_expect = < stdio : string ; v : int >  Js.t
external config1 : stdio:(_ [@bs.as "inherit"]) -> v:int -> unit ->  _  = "" [@@bs.obj]

let v1 : config1_expect = config1 ~v:3 ()

type config2_expect = < stdio : int ; v : int >  Js.t
external config2 : stdio:(_ [@bs.as 1 ]) -> v:int -> unit ->  _  = "" [@@bs.obj]

let v2 : config2_expect = config2 ~v:2 () 


external on_exit : 
    (_ [@bs.as "exit"]) -> 
    (int -> string) -> 
    unit = "process.on" 
    [@@bs.val]

let () = 
    on_exit (fun exit_code -> string_of_int exit_code)    


external on_exit_int : 
    (_ [@bs.as 1]) -> 
    (int -> unit) -> 
    unit = "process.on"      
    [@@bs.val]

let () = 
    on_exit_int (fun _ -> ())     

external on_exit3 :     (int -> string ) -> (_ [@bs.as "exit"]) -> unit = 
    "process.on" 
    [@@bs.val]

let ()  = on_exit3 (fun i -> string_of_int i )  

external on_exit4 :     (int -> string ) -> (_ [@bs.as 1]) -> unit = 
    "process.on" 
    [@@bs.val]


let () = 
    on_exit4 (fun i -> string_of_int i)

external on_exit_slice : 
    int -> (_ [@bs.as 3]) -> (_ [@bs.as "xxx"]) -> string array -> unit = 
    "xx"   [@@bs.val] [@@bs.splice]

let () =     
    on_exit_slice 3 [|"a";"b"|]        



type t 

external on_exit_slice1 : 
    int -> int array -> unit = "xx" [@@bs.send.pipe: t]

external on_exit_slice2 : 
    int 
    -> (_ [@bs.as 3]) 
    -> (_ [@bs.as "xxx"]) -> int array -> unit = 
    "xx"    [@@bs.send.pipe: t]

external on_exit_slice3 : 
    int 
    -> (_ [@bs.as 3]) 
    -> (_ [@bs.as "xxx"]) 
    -> int array
    -> unit 
    = 
    "xx"    [@@bs.send.pipe: t] [@@bs.splice]

external on_exit_slice4 : 
    int 
    -> (_ [@bs.as 3]) 
    -> (_ [@bs.as "xxx"]) 
    -> ([`a|`b|`c] [@bs.int])
    -> ([`a|`b|`c] [@bs.string])
    -> int array
    -> unit 
    = 
    "xx" [@@bs.send.pipe: t] [@@bs.splice]


external on_exit_slice5 : 
    int 
    -> (_ [@bs.as 3]) 
    -> (_ [@bs.as {json|true|json}])
    -> (_ [@bs.as {json|false|json}])
    -> (_ [@bs.as {json|"你好"|json}])
    -> (_ [@bs.as {json| ["你好",1,2,3] |json}])
    -> (_ [@bs.as {json| [{ "arr" : ["你好",1,2,3], "encoding" : "utf8"}] |json}])
    -> (_ [@bs.as "xxx"]) 
    -> ([`a|`b|`c] [@bs.int])
    -> (_ [@bs.as "yyy"]) 
    -> ([`a|`b|`c] [@bs.string])
    -> int array
    -> unit 
    = 
    "xx" [@@bs.send.pipe: t] [@@bs.splice]


(**
 TODO: bs.send conflicts with bs.val: better error message
*)
let f (x : t) = 
    x |> on_exit_slice1 __LINE__ [|1;2;3|];
    x |> on_exit_slice2 __LINE__ [|1;2;3|];
    x |> on_exit_slice3 __LINE__ [|1;2;3|];
    x |> on_exit_slice4 __LINE__ `a `b [|1;2;3;4;5|];
    x |> on_exit_slice5 __LINE__ `a `b [|1;2;3;4;5|]

external process_on_exit : (_ [@bs.as "exit"]) -> (int -> unit) -> unit =
  "process.on" [@@bs.val]

let () = 
    process_on_exit (fun exit_code -> 
        Js.log( "error code: " ^ string_of_int exit_code ))      


type process

external on_exit :  (_ [@bs.as "exit"]) -> (int -> unit) -> unit = 
    "on" [@@bs.send.pipe: process]
let register (p : process) = 
        p |> on_exit (fun i -> Js.log i )


external io_config : 
    stdio:( _ [@bs.as "inherit"]) -> cwd:string -> unit -> _  = "" [@@bs.obj]

let config = io_config ~cwd:"." ()   

