
let result = ref ""
module Xx = struct 
  let log x =  result := x 
end 
(** TODO: 
  pattern match over (Some "xx") could be simplified
*)
let compilerBug a b c f =
  match (a, b) with
  | (((Some ((("x"))))),_)
    |(_,((Some ((("x")))))) ->
      if f ()
      then
        Xx.log
          (("Some x, f returns true"))
      else
        Xx.log
          (("Some x, f returns false"))
  | _ ->
      if c
      then
        Xx.log (("No x, c is true"))
      else
        Xx.log (("No x, c is false"))


#if 1 then 
let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = Mt.eq_suites ~test_id ~suites loc x y 

let _ =  
  compilerBug ((Some ((("x")))))
    None true (fun ()  -> true);
  eq __LOC__ !result  "Some x, f returns true"

let () =    
  Mt.from_pair_suites __FILE__ !suites
#end  