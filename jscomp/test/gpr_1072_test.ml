

external ice_cream:
    ?flavor:([`vanilla | `chocolate ] [@bs.string]) -> 
    num:int ->
    unit -> 
    _ =  ""
[@@bs.obj]


let my_scoop = ice_cream ~flavor:`vanilla ~num:3 ()

(*
external ice_cream_2:
    flavor:([`vanilla | `chocolate ] [@bs.string]) -> 
    num:int ->
    unit -> 
    _ =  ""
[@@bs.obj]

let my_scoop2 = ice_cream_2 ~flavor:`vanilla ~num:3 ()
*)

type opt_test = < x : int Js.Undefined.t ; y : int Js.Undefined.t> Js.t
external opt_test : ?x:int -> ?y:int -> unit -> _ = "" 
[@@bs.obj]


let u : opt_test = opt_test ~y:3 ()

