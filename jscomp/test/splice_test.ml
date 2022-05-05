let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = Mt.eq_suites ~test_id ~suites loc x y 


module Caml_splice_call = struct 
end 
external f : int -> int array -> int = "Math.max" 
  [@@bs.splice] [@@bs.val]

;; f 1 [||] |. ignore

external send : int -> int array -> int = "send" 
  [@@bs.splice] [@@bs.send]

let f00 a b =   
  a |. send [|b|]

external push : int array -> int -> int array -> unit = 
  "push" [@@bs.send] [@@bs.splice]

(* This is only test, the binding maybe wrong
  since in OCaml array'length is not mutable
*)
let () =   
  let a = [||] in 
  a |. push 1 [|2;3;4|];

  eq __LOC__ a [|1;2;3;4|]

let dynamic arr =   
  let a = [||] in 
  a |. push 1 arr ;
  eq __LOC__ a (Array.concat [[|1|]; arr])

;; dynamic [|2;3;4|]  
;; dynamic [||]
;; dynamic [|1;1;3|]

(* Array constructor with a single parameter `x`
   just makes an array with its length set to `x`,
   so at least two parameters are needed
*)
external newArr : int -> int -> int array -> int array = "Array"
  [@@bs.splice] [@@bs.new]

let () =
  let a = newArr 1 2 [|3;4|] in
  eq __LOC__ a [|1;2;3;4|]

let dynamicNew arr =
  let a = newArr 1 2 arr in
  eq __LOC__ a (Array.concat [[|1; 2|]; arr])

;; dynamicNew [|3;4|]
;; dynamicNew [||]
;; dynamicNew [|1;3|]

[%%raw{|
class Foo {
  constructor(...names) {
    this.names = names;
  }
}
|}]

type foo

external newFoo : string array -> foo = "Foo" [@@bs.splice] [@@bs.new]
external fooNames : foo -> string array = "names" [@@get]

let () =
  let f = newFoo [|"a";"b";"c"|] in
  eq __LOC__ (fooNames f) [|"a";"b";"c"|]

let dynamicFoo arr =
  let f = newFoo arr in
  eq __LOC__ (fooNames f) arr

;; dynamicFoo [||]
;; dynamicFoo [|"a"|]
;; dynamicFoo [|"a";"b";"c"|]

module Pipe = struct
  external push :  int array -> int -> int array -> unit = 
    "push" [@@send] [@@bs.splice] 

  (* This is only test, the binding maybe wrong
     since in OCaml array'length is not mutable
  *)
  let () =   
    let a = [||] in 
    a |. push 1 [|2;3;4|];

    eq __LOC__ a [|1;2;3;4|]

  let dynamic arr =   
    let a = [||] in 
    a |. push 1 arr ;
    eq __LOC__ a (Array.concat [[|1|]; arr])

  ;; dynamic [|2;3;4|]  
  ;; dynamic [||]
  ;; dynamic [|1;1;3|]

end 

#if 1 then
let f1 (c : int array) =  f 1 c 

;; eq __LOC__ (f1  [|2;3|] ) 3 
;; eq __LOC__ (f1  [||] ) 1
;; eq __LOC__ (f1 [|1;2;3;4;5;2;3|]) 5
#end

;; Mt.from_pair_suites __FILE__ !suites