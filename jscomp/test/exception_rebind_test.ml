
module A = struct 
  exception E
end


module B = struct 
  exception F =  A.E
end

exception H = Exception_def.A


type u = exn 

type exn += A0 of int 
#if 0 then
type u += A1 of int (*Type definition u is not extensible*)
#end