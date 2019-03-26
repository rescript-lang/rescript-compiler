let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = 
  incr test_id ; 
  suites := 
    (loc ^" id " ^ (string_of_int !test_id), (fun _ -> Mt.Eq(x,y))) :: !suites

(**
[%%raw [@import "jscomp/test/xx.js"]] (* PATH relative to package.json *)
*)

[%%raw{|
function Make (){
  this.data = []
  for(var i = 0; i < arguments.length; ++i){
   this.data[i] = arguments[i]
}
}

Make.prototype.sum = function(){
  var result  = 0;
  for(var k = 0; k < this.data.length; ++k){
    result = result + this.data[k]
  };
  return result
}  

Make.prototype.add = function(){
  
} 
|}]

type t 


external make : int -> int -> int -> int -> t = "Make"  [@@bs.new]


external sum : t -> unit -> int = "" [@@bs.send]

(* compile error *)
(* external join : string  -> string = "" [@@bs.module "path"] [@@bs.splice] *)
external join : string array -> string = "" [@@bs.module "path"] [@@bs.splice]

external test : string array -> t = "" [@@bs.send.pipe: t ] [@@bs.splice]

(* compile error *)
(* external test2 : int -> string -> t= "" [@@bs.send.pipe: t ] [@@bs.splice] *)
let u = [|"x";"d" |]
let f x  = 
  x 
  |> test  [| "a"; "b" |]
  |> test  [| "a"; "b" |]
  (* |> test u *)

let v = make 1 2 3 4

let u = sum v ()


let () = eq __LOC__ u 10

;; Mt.from_pair_suites __MODULE__ !suites
