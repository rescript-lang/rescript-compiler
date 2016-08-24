[%%bs.raw{|
function hey_string (option){
  switch(option){
  case "on_closed" : return 1 ;
  case "on_open" : return 2 ; 
  case "in" : return 3;
  default : throw Error ("impossible")
 }
}
function hey_int (option){
  switch (option){
   case 0 : return 1;
   case 3 : return 3;
   case 4 : return 4;
   default : throw Error("impossible")
  }
 }
|}]

type u = [`on_closed | `on_open | `in_ [@bs.as "in"]]

(** when marshall, make sure location does not matter *)
external test_string_type : 
  ([`on_closed | `on_open | `in_ [@bs.as "in"]]
                [@bs.string]) -> int  = 
  "hey_string" [@@bs.val]

external test_int_type : 
  ([`on_closed | `on_open [@bs.as 3] | `in_ ]
                [@bs.int])  -> int  = 
  "hey_int" [@@bs.val]

let uu =
  [| test_string_type `on_open; test_string_type `on_closed; test_string_type `in_ |]

let vv = 
  [| test_int_type `on_open; test_int_type `on_closed; test_int_type `in_ |]



let option = `on_closed 

let v = test_string_type option 

let ff h  = test_string_type h

let xx = test_string_type `in_

type readline
external on : 
  readline -> 
  ([ `line of (string -> unit [@bs]) 
   | `close of (unit -> unit [@bs])] 
     [@bs.string]) ->
  unit = 
  "" [@@bs.send]


let register readline = 
  on readline (`line begin fun[@bs] s -> Js.log s end);
  on readline (`close begin fun[@bs] () -> Js.log "finished" end)

(* external on :  *)
(*       ([ `line of (string -> unit [@bs])  *)
(*        | `close of (unit -> unit [@bs])]  *)
(*          [@bs.string]) ->  *)
(*       readline -> readline  =  *)
(*   "on" [@@bs.send] *)
external on2 : 
  readline -> 
  ([ `line of (string -> unit [@bs]) 
   | `close of (unit -> unit [@bs])] 
     [@bs.string]) ->
  unit = 
  "" [@@bs.send]

external readFileSync :
  string -> ([`utf8 | `ascii] [@bs.string]) ->
  string = ""
  [@@bs.val]
  [@@bs.module "fs"]

let read name = 
  readFileSync name `utf8

module N = struct
  external readFileSync :
    string -> ([`utf8 | `ascii] [@bs.string]) ->
    string = ""
      [@@bs.module "fs"]
  let read name = 
    readFileSync name `utf8

end  
let readN = N.read
(**
let register readline = 
  readline 
  |> on (`line begin fun [@bs] s -> Js.log s end)
  |> on (`close begin fun [@bs] () -> Js.log "finished" end)

{[
let register readline = 
  on (`line begin fun [@bs] s -> Js.log s end) readline; 
  on (`close begin fun [@bs] () -> Js.log "finished" end) readline

]}
*)
let test readline x  = 
  on readline x 
