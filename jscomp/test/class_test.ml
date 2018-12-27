
class point =
    object
      val mutable x = 0
      method get_x = x
      method move d = x <- x + d
    end;;

let p = new point

let zero =  p#get_x (* 0 *)


let () =  p#move 3;;


let three =  p#get_x (* 3 *)


let x0 = ref 0;;

class point2 =
    object
      val mutable x = incr x0; !x0
      method get_x = x
      method move d = x <- x + d
    end;;

let one = new point2#get_x 
let two = new point2#get_x

;; Mt.from_pair_suites __MODULE__ Mt.[
    __LOC__, (fun _ -> Eq (zero, 0));
    __LOC__, (fun _ -> Eq (three, 3));
    __LOC__, (fun _ -> Eq (one, 1));
    __LOC__, (fun _ -> Eq (two, 2));
]

