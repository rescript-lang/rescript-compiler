
type t = A of int | B of string
let eq_A (x :t) y = 
  match x with
  | A x -> 
    (match y with A x1 -> x = x1 | _ -> false)
   | _ -> false  

module Test() =
struct
  let () = Js.log "no inline"
  let u = A 3
  module Block = struct  end
  let y = 32
  let b = eq_A (A 3) u  
end


module Test2() =
struct
  let () = Js.log "no inline"

  module Block = struct  end
  let y = 32
  let b = eq_A (A 3) (A 3)
end


let x = 3

let f i y =
  let x = A i  in 
  eq_A x y

module Test3 () = struct  
  let f x y = x = y
  module Caml_obj = struct 
  end 
end
module Test4 () = struct 
  module Caml_obj = struct 
  end 
  let f x y = x = y
end 


module Test5 () = struct
  let f x = Some x 
  module Caml_option = struct   
  end 
end 

module Test6 () = struct 
  module Caml_option = struct   
  end 
  let f x = Some x 
end

module Test7 () = struct 
  module Caml_option = struct
  end 
end

module Test8 () = struct 
  module Curry = struct   
  end 
  let f x = x 1 
end 

module Test9 () = struct 
  let f x = x 1 
  module Curry = struct   
  end 
end 
