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
class colored_point x (c : string) =
    object
      inherit point x
      val c = c
      method color = c
    end;;

let colored_point_to_point cp = (cp : colored_point :> point);;

let p = new point 3 and q = new colored_point 4 "blue";;

let rec lookup_obj obj =
  function
   | [] -> raise Not_found
   | obj' :: l ->
     if (obj :> < >) = (obj' :> < >) then obj' else lookup_obj obj l ;;




class type c' = object method m : int end;;

class c : c' = object method m = 1 end
  and d = object (self)
    inherit c
    method n = 2
    method as_c = (self :> c')
  end;;

class virtual c2' = object method virtual m : int end;;


class functional_point y =
    object
      val x = y
      method get_x = x
      method move d = {< x = x + d >}
    end;;




let () = 
  let p = new functional_point 7 in 
  eq __LOC__ 
    (7,10,7)
    (p#get_x, (p#move 3)#get_x , p#get_x)


class bad_functional_point y =
    object
      val x = y
      method get_x = x
      method move d = new bad_functional_point (x+d)
    end;;

let () = 
  let p = new bad_functional_point 7 in 
  eq __LOC__
    (7,10,7)
    (p#get_x, (p#move 3)#get_x , p#get_x)

let () =  Mt.from_pair_suites __MODULE__ !suites
