let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = 
  incr test_id ; 
  suites := 
    (loc ^" id " ^ (string_of_int !test_id), (fun _ -> Mt.Eq(x,y))) :: !suites

class point = fun x_init ->
    object
      val mutable x = x_init
      method get_x = x
      method move d = x <- x + d
    end

let () = 
  let p = new point 55 in 
  (* TODO: see why [field 1] is missing 
  *)
  (* let () = [%bs.debug] in  *)
  let q = Oo.copy p in 
  q # move 7;
  eq __LOC__ (55, 62) (p#get_x, q # get_x )

let () = Mt.from_pair_suites __FILE__ !suites
