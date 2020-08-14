
module N = struct
  type t = {
    a : int; 
    b : int;
    c : int
  }
end 

let f (e : N.t) = 
  let {a;b;c} = e in 
   a + b +c   