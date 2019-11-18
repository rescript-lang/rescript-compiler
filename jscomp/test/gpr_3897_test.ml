module type S = sig 
  val x : int 
end 




module Make (U : S) = struct 
  include U
end 