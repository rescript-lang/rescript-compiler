
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