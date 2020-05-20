
let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = Mt.eq_suites ~test_id ~suites loc x y 
let magic = 33
class c1 =
  object (self)
    method b =
      object
        method c =
          object
            method d = self # b
          end
        method h = magic
      end
  end

class c2 =
  object
    inherit c1
    method a = ()
  end

let _ = new c2 # b
let e = new c1 # b # c # d # h ;;
eq __LOC__ e magic

;; Mt.from_pair_suites __FILE__ !suites