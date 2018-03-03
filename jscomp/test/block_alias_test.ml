

module Block = struct 
end 
type t = 
  | A0 of int
  | A of int * int 

let v0 = A (0,1)


module N = struct 
  module Block = struct end
  let v1 = A (0,1) 
end 