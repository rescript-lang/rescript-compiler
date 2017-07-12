
module N  = struct
  let add x y = x + y
end

module type S0 =  sig 
  val f1 : unit -> unit 
  val f2 : unit -> unit -> unit 
  val f3 : unit -> unit -> unit -> unit 
end 

module N0 : S0 = struct 
  let f4 _ _ _ = ()
  let f1 _ = ()
  let f2 _ _ = ()
  let f3 _ _ _ = ()
end

module N1 = struct 
  let f4 _ _ _ = ()
  let f1 _ = ()
  let f2 _ _ = ()
  let f3 _ _ _ = ()
end