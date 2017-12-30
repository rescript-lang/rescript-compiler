let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = Mt.eq_suites ~test_id ~suites loc x y 


class virtual comparable =
    object (_ : 'a)
      method virtual leq : 'a -> bool
    end;;

class money (x : float) =
    object
      inherit comparable
      val repr = x
      method value = repr
      method leq p = repr <= p#value
    end;;


class money2 x =
    object
      inherit money x
      method times k = {< repr = k *. repr >}
    end;;

let min (x : #comparable) y =
    if x#leq y then x else y;;

let () = 
  eq __LOC__ 
    1.
    (min (new money  1.0) (new money 3.0))#value;;

let () = 
  eq __LOC__
    3.
  (* Js.log  *)
    (min (new money2 5.0) (new money2 3.))#value;;

let () = Mt.from_pair_suites __FILE__ !suites
