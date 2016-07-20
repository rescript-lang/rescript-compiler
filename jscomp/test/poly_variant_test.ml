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
  "hey_string" [@@bs.call]

external test_int_type : 
  ([`on_closed | `on_open [@bs.as 3] | `in_ ]
                [@bs.int])  -> int  = 
  "hey_int" [@@bs.call]

let uu =
  [| test_string_type `on_open; test_string_type `on_closed; test_string_type `in_ |]

let vv = 
  [| test_int_type `on_open; test_int_type `on_closed; test_int_type `in_ |]



let option = `on_closed 

let v = test_string_type option 

let ff h  = test_string_type h

let xx = test_string_type `in_


