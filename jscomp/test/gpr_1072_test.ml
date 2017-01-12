
(*
external ice_cream:
    ?flavor:([`vanilla | `chocolate ] [@bs.string]) -> 
    num:int ->
    unit -> 
    _ =  ""
[@@bs.obj]


let my_scoop = ice_cream ~flavor:`vanilla ~num:3 ()
*)
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


type u
external ice_cream3:
    ?flavor:([`vanilla | `chocolate ] [@bs.string]) -> 
    num:int ->
    unit -> 
    u =  ""
[@@bs.val]


external label_test : x_ignore:int -> unit -> _ = "" [@@bs.obj]

(** here the type label should be the same, 
    when get the object, it will be mangled *)
type label_expect = < x_ignore : int > Js.t 

let vv : label_expect = label_test ~x_ignore:3 ()

let v = ice_cream3 ~flavor:`vanilla ~num:3 ()


external int_test : x_ignore:([`a|`b] [@bs.int]) -> unit -> _ = "" [@@bs.obj]
(* translate [`a] to 0, [`b] to 1 *)
type int_expect = < x_ignore : int > Js.t 

let int_expect : int_expect = int_test ~x_ignore:`a ()

external int_test2 : ?x_ignore:([`a|`b] [@bs.int]) -> unit -> _ = "" [@@bs.obj]

let int_expect2 = int_test2 ~x_ignore:`a ()

external ice :
  flavour:([`vanilla | `chocolate] [@bs.string]) ->
  num:int -> unit -> 
  _ =
  "" [@@bs.obj]

let mk_ice = ice ~flavour:`vanilla ~num:3 ()

external ice2 :
  ?flavour:([`vanilla | `chocolate] [@bs.string]) ->
  num:int -> unit -> 
  _ =
  "" [@@bs.obj]
  
let my_ice2 = ice2 ~flavour:`vanilla ~num:1  ()

let my_ice3 = ice2 ~num:2 ()


external mk4:x_ignore:([`a|`b][@bs.ignore]) -> y:int -> unit -> _ = "" [@@bs.obj]

let v_mk4 = mk4 ~x_ignore:`a ~y:3 ()

external mk5: x:unit -> y:int -> unit -> _ = "" [@@bs.obj]

let v_mk5 = mk5 ~x:() ~y:3 ()

external mk6: ?x:unit -> y:int -> unit -> _ = "" [@@bs.obj]

let v_mk6 = mk6 ~y:3 ()

let v_mk6_1 = mk6 ~x:() ~y:3 ()

external mk :  ?x_ignore:([`a|`b] [@bs.int]) -> unit -> _ = "" [@@bs.val]



(* TODO: fix me *)
let mk_u = mk ~x_ignore:`a ()


external again : ?x_ignore:([`a|`b][@bs.string]) -> int -> unit = "" [@@bs.val]

let () = again ~x_ignore:`a 3 

external again2 : x_ignore:([`a|`b][@bs.string]) -> int -> unit = "" [@@bs.val]

let () = again2 ~x_ignore:`a 3 

external again3 : x_ignore:([`a|`b][@bs.ignore]) -> int -> unit = "" [@@bs.val]

let () = again3 ~x_ignore:`a 3

external again4 : ?x:unit -> y:unit -> unit -> unit  = "" [@@bs.val]

let side_effect = ref 0        
let () = 
        again4 ~x:() ~y:() ();
        again4 ~y:() ();
        again4 ~x:(incr side_effect) ~y:() ();
        again4 ~x:(incr side_effect) ~y:(decr side_effect) ()



