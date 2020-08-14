
module N = struct
  type 'a t = {
    a : int; 
    b : int;
    c : int;
    d : 'a
  }
end 

let f (e : _ N.t) = 
  let {a;b;c} = e in 
   a + b +c   

type e = (int -> int [@bs]) N.t 

let f1 (e : e) = 
  let {a;b ; c ; d } = e in    
  a + b + c + (d c [@bs])

   