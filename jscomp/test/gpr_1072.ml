(* external ice_cream: ?flavor:([`vanilla | `chocolate ] [@bs.string]) ->
   num:int -> unit -> _ = "" [@@bs.obj]

   let my_scoop = ice_cream ~flavor:`vanilla ~num:3 () *)
(* external ice_cream_2: flavor:([`vanilla | `chocolate ] [@bs.string]) ->
   num:int -> unit -> _ = "" [@@bs.obj]

   let my_scoop2 = ice_cream_2 ~flavor:`vanilla ~num:3 () *)

type opt_test = < x: int Js.Undefined.t ; y: int Js.Undefined.t > Js.t

external opt_test : ?x:int -> ?y:int -> unit -> _ = "" [@@bs.obj]

let u : opt_test = opt_test ~y:3 ()

external ice_cream3 :
     ?flavor:([`vanilla | `chocolate[@bs.as "x"]][@bs.string])
  -> num:int
  -> unit
  -> _ = ""
  [@@bs.obj]

(* TODO: warn when [_] happens in any place except `bs.obj` *)

type ice_cream3_expect = < flavor: string Js.undefined ; num: int > Js.t

let v_ice_cream3 : ice_cream3_expect list =
  [ ice_cream3 ~flavor:`vanilla ~num:3 ()
  ; ice_cream3 ~flavor:`chocolate ~num:3 ()
  ; ice_cream3 ~flavor:`vanilla ~num:3 () ]

type u

external ice_cream4 :
     ?flavor:([`vanilla | `chocolate[@bs.as "x"]][@bs.string])
  -> num:int
  -> unit
  -> u = ""
  [@@bs.obj]

let v_ice_cream4 : u list =
  [ ice_cream4 ~flavor:`vanilla ~num:3 ()
  ; ice_cream4 ~flavor:`chocolate ~num:3 () ]

external label_test : x__ignore:int -> unit -> _ = "" [@@bs.obj]

type label_expect = < x__ignore: int > Js.t
(** here the type label should be the same, when get the object, it will be
    mangled *)

let vv : label_expect = label_test ~x__ignore:3 ()

external int_test : x__ignore:([`a | `b][@bs.int]) -> unit -> _ = "" [@@bs.obj]

(* translate [`a] to 0, [`b] to 1 *)
type int_expect = < x__ignore: int > Js.t

let int_expect : int_expect = int_test ~x__ignore:`a ()

external int_test2 : ?x__ignore:([`a | `b][@bs.int]) -> unit -> _ = ""
  [@@bs.obj]

type int_expect2 = < x__ignore: int Js.Undefined.t > Js.t

let int_expect2 : int_expect2 = int_test2 ~x__ignore:`a ()

external int_test3 : ?x__ignore:([`a[@bs.as 2] | `b][@bs.int]) -> unit -> _
  = ""
  [@@bs.obj]

let int_expects : int_expect2 list =
  [int_test3 (); int_test3 ~x__ignore:`a (); int_test3 ~x__ignore:`b ()]

external ice :
  flavour:([`vanilla | `chocolate][@bs.string]) -> num:int -> unit -> _ = ""
  [@@bs.obj]

let mk_ice : < flavour: string ; num: int > Js.t =
  ice ~flavour:`vanilla ~num:3 ()

external ice2 :
  ?flavour:([`vanilla | `chocolate][@bs.string]) -> num:int -> unit -> _ = ""
  [@@bs.obj]

let my_ice2 : < flavour: string Js.Undefined.t ; num: int > Js.t =
  ice2 ~flavour:`vanilla ~num:1 ()

let my_ice3 : < flavour: string Js.Undefined.t ; num: int > Js.t =
  ice2 ~num:2 ()

external mk4 : x__ignore:([`a | `b][@bs.ignore]) -> y:int -> unit -> _ = ""
  [@@bs.obj]

let v_mk4 : < y: int > Js.t = mk4 ~x__ignore:`a ~y:3 ()

external mk5 : x:unit -> y:int -> unit -> _ = "" [@@bs.obj]

let v_mk5 : < x: unit ; y: int > Js.t = mk5 ~x:() ~y:3 ()

external mk6 : ?x:unit -> y:int -> unit -> _ = "" [@@bs.obj]

let v_mk6 : < x: unit Js.Undefined.t ; y: int > Js.t = mk6 ~y:3 ()
let v_mk6_1 = mk6 ~x:() ~y:3 ()

type mk

external mk : ?x__ignore:([`a | `b][@bs.int]) -> unit -> _ = "" [@@bs.obj]

(* TODO: fix me *)
let mk_u : < x__ignore: int Js.Undefined.t > Js.t = mk ~x__ignore:`a ()

external mk7 : ?x:([`a | `b][@bs.ignore]) -> y:int -> unit -> _ = "" [@@bs.obj]

let v_mk7 : < y: int > Js.t list =
  [mk7 ~x:`a ~y:3 (); mk7 ~x:`b ~y:2 (); mk7 ~y:2 ()]

external again : ?x__ignore:([`a | `b][@bs.string]) -> int -> unit = "again"
  [@@bs.val]

let () =
  again ~x__ignore:`a 3 ;
  again 3 ;
  again ?x__ignore:None 3 ;
  again ?x__ignore:(ignore 3 ; None) 3

external again2 : x__ignore:([`a | `b][@bs.string]) -> int -> unit = "again2"
  [@@bs.val]

let () = again2 ~x__ignore:`a 3

external again3 : x__ignore:([`a | `b][@bs.ignore]) -> int -> unit = "again3"
  [@@bs.val]

let () =
  again3 ~x__ignore:`a 3 ;
  again3 ~x__ignore:`b 2

external again4 : ?x:unit -> y:unit -> int -> unit -> unit = "again4"
  [@@bs.val]

let side_effect = ref 0

let () =
  again4 ~y:() __LINE__ () ;
  again4 ?x:None ~y:() __LINE__ () ;
  again4 ?x:(Some ()) ~y:() __LINE__ () ;
  again4 ~x:() ~y:() __LINE__ () ;
  again4 ~y:() __LINE__ () ;
  again4 ~x:(incr side_effect ; ()) ~y:() __LINE__ () ;
  again4 ~x:(incr side_effect ; ()) ~y:(decr side_effect ; ()) __LINE__ () ;
  again4 ~y:(decr side_effect ; ()) __LINE__ () ;
  again4 ~x:(incr side_effect) ~y:() __LINE__ ()

(* external again5 : ?x__ignore:([`a of unit -> int | `b of string -> int ]
   [@bs.string]) *)
(* -> int -> unit = "" [@@bs.val] *)

(* let v = again5 3 *)
