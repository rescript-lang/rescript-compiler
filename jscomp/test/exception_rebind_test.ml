
module A = struct 
  exception E
end


module B = struct 
  exception F =  A.E
end

exception H = Exception_def.A


type u = exn 

type exn += A0 of int 

exception H0 = Invalid_argument

exception H1 = H0 

let u0 = H1 "x"

let u1 = Invalid_argument "x"
let u2 = Not_found
#if 0 then
type u += A1 of int (*Type definition u is not extensible*)
#end