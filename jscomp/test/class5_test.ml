let suites : Mt.pair_suites ref = ref []
let test_id = ref 0

let eq loc x y =
  incr test_id ;
  suites :=
    (loc ^ " id " ^ string_of_int !test_id, fun _ -> Mt.Eq (x, y)) :: !suites

class printable_point x_init =
  object (s)
    val mutable x = x_init

    method get_x = x

    method move d = x <- x + d

    method print = string_of_int s#get_x
  end

class printable_colored_point y c =
  object (self)
    val c = c

    method color = c

    inherit printable_point y as super

    method print = "(" ^ super#print ^ ", " ^ self#color ^ ")"
  end

let p = new printable_colored_point 17 "red"
let () = eq __LOC__ p#print "(17, red)"

class ['a] ref x_init =
  object
    val mutable x : 'a = x_init

    method get = x

    method set y = x <- y
  end

let v =
  let r = new ref 1 in
  r#set 2 ; r#get

let () = eq __LOC__ v 2

class ['a] intlist (l : int list) =
  object
    method empty = l = []

    method fold f (accu : 'a) = List.fold_left f accu l
  end

let l = new intlist [1; 2; 3]
let () = eq __LOC__ 6 (l#fold (fun x y -> x + y) 0)

class intlist2 (l : int list) =
  object
    method empty = l = []

    method fold : 'a. ('a -> int -> 'a) -> 'a -> 'a =
      fun f accu -> List.fold_left f accu l
  end

let l = new intlist2 [1; 2; 3]

let () =
  eq __LOC__ (6, "1 2 3 ")
    ( l#fold (fun x y -> x + y) 0
    , l#fold (fun s x -> s ^ string_of_int x ^ " ") "" )

class type point0 =
  object
    method get_x : int
  end

class point x_init =
  object
    val mutable x = x_init

    method get_x = x

    method move d = x <- x + d
  end

class distance_point x =
  object
    inherit point x

    method distance : 'a. (#point0 as 'a) -> int =
      fun other -> abs (other#get_x - x)
  end

let a, b =
  let p = new distance_point 3 in
  (p#distance (new point 8), p#distance (new printable_colored_point 1 "blue"))

let () = eq __LOC__ (5, 2) (a, b)
let () = Mt.from_pair_suites __MODULE__ !suites
