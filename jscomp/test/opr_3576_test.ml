let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = Mt.eq_suites ~test_id ~suites loc x y 


class a =
  object (self)
    method m1 = object method m3 = 3 end
    method m2 =
      object
        method m4 = self#m1
      end
  end

class b =
  object (self)
    inherit a
    method a_text () = ()
  end

let _ = (new a)#m2
;; eq __LOC__ ((new b)#m2#m4#m3) 3 

;; Mt.from_pair_suites __FILE__ !suites