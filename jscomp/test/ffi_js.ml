let keys :  Obj.t -> string array [@bs] = [%bs.raw " function (x){return Object.keys(x)}" ]



[%%bs.raw{|
  function $$height_order(x){
   return function(y,z){
      return x + y + z 
   }
  }
|}]
external high_order: int -> (int -> int -> int  [@bs]) = "$$high_order" [@@bs.val]


let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc (x, y) = 
  incr test_id ; 
  suites := 
    (loc ^" id " ^ (string_of_int !test_id), (fun _ -> Mt.Eq(x,y))) :: !suites


let () = 
  eq __LOC__ (6, ((high_order 1 ) 2 3 [@bs]))

let () = Mt.from_pair_suites __FILE__ !suites
