let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = 
  incr test_id ; 
  suites := 
    (loc ^" id " ^ (string_of_int !test_id), (fun _ -> Mt.Eq(x,y))) :: !suites


class restricted_point x_init =
    object (self)
      val mutable x = x_init
      method get_x = x
      method private move d = x <- x + d
      method bump = self#move 1
    end;;


class type restricted_point_type =
    object
      method get_x : int
      method bump : unit
  end;;

class restricted_point' x = (restricted_point x : restricted_point_type);;

class restricted_point2' x = (restricted_point x : restricted_point_type);;

module type POINT = sig
    class restricted_point' : int ->
      object
        method get_x : int
        method bump : unit
      end
  end;;

module Point : POINT = struct
    class restricted_point' = restricted_point
end;;


class virtual abstract_point x_init =
    object (self)
      method virtual get_x : int
      method get_offset = self#get_x - x_init
      method virtual move : int -> unit
    end;;

class point x_init =
    object
      inherit abstract_point x_init
      val mutable x = x_init
      method get_x = x
      method move d = x <- x + d
    end;;


class colored_point x (c : string) =
    object
      inherit point x
      val c = c
      method color = c
    end;;

let p' = new colored_point 5 "red";;

let () = 
  eq __LOC__ (5, "red") (p'#get_x, p'#color);; 

let get_succ_x p = p#get_x + 1;;

let () = eq __LOC__ 6 (get_succ_x p');;

let set_x p = p#set_x;;

let incr p = set_x p (get_succ_x p)


let () =  Mt.from_pair_suites __MODULE__ !suites
