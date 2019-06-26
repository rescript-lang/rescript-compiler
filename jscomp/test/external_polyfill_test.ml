let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = Mt.eq_suites ~test_id ~suites loc x y 


external ff : int -> int -> int = "caml_fancy_add"


[%%raw{|
require('../../lib/js/caml_external_polyfill.js').register("caml_fancy_add", function(x,y){
  return + ((""+x ) + (""+y))
})
|}]


let h = ff 1 2 

let ()= 
  eq __LOC__ h 12


let () = Mt.from_pair_suites __MODULE__ !suites
