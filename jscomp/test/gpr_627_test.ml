let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc (x, y) = 
  incr test_id ; 
  suites := 
    (loc ^" id " ^ (string_of_int !test_id), (fun _ -> Mt.Eq(x,y))) :: !suites


let u = object
  method say x y = x + y
end [@bs]

let v =
  let x = 3. in
  object (self)
    method hi x y =
      let u = [%bs.obj{ x  }] in      
      self##say u##x +. y +.x                            
    method say  = fun x ->  x *. self## x ()
    method x () = x
  end [@bs]


let () =
  let p = (3, u##say 1 2) in
  eq __LOC__ p ;
  eq __LOC__ (v##hi 1. 2., 6.)


let () = Mt.from_pair_suites __MODULE__ !suites

