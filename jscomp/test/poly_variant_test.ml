let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = 
  incr test_id ; 
  suites := 
    (loc ^" id " ^ (string_of_int !test_id), (fun _ -> Mt.Eq(x,y))) :: !suites

[%%bs.raw{|
function hey_string (option){
  switch(option){
  case "on_closed" : 
  case "on_open" : 
  case "in" : return option
  default : throw Error ("impossible")
 }
}
function hey_int (option){
  switch (option){
   case 0 : 
   case 3 : 
   case 4 : 
   case 5:
   case 6 : return option
   default : throw Error("impossible")
  }
 }
|}]

type u = [`on_closed | `on_open | `in_ 
            (* [@bs.as "in"] TODO: warning test  *)]
(* indeed we have a warning here*)
(* TODO: add warning test 
*)
(** when marshall, make sure location does not matter *)
external test_string_type : 
  flag:([`on_closed | `on_open | `in_ [@bs.as "in"]]
                [@bs.string]) -> string  = 
  "hey_string" [@@bs.val]

external test_int_type : 
 ([`on_closed 
   | `on_open [@bs.as 3] 
   | `in_
   | `again [@bs.as 5]
   | `hey
   ]
                [@bs.int])  -> int  = 
  "hey_int" [@@bs.val]

external test_string_extended_closed :
  flag:([> u] [@bs.string]) -> string  =
  "hey_string" [@@bs.val]

let uu =
  [| test_string_type ~flag: `on_open; test_string_type ~flag: `on_closed; 
     test_string_type ~flag: `in_ |]

let vv = 
  [| test_int_type `on_open; test_int_type `on_closed; test_int_type `in_ |]

let ww =
  [| test_string_extended_closed ~flag: `on_open; test_string_extended_closed ~flag: `on_closed;
     test_string_extended_closed ~flag: `in_ |]

let () =
  eq __LOC__ vv [|3;0;4|];
  eq __LOC__ (test_int_type `again, test_int_type `hey) (5,6);
  eq __LOC__ uu [|"on_open"; "on_closed"; "in"|]
  eq __LOC__ ww [|"on_open"; "on_closed"; "in"|]


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

let () = Mt.from_pair_suites __MODULE__ !suites
