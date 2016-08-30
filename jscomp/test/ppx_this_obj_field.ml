

let v5 x =
  object(self)
    val  x = 3
    val mutable y = 4
    method (* private *) setY v  =  
      self##y #= 2 ;
      self##y, v
    method say () =
      self##x + self##y
  end [@bs]

(* let v6 x = object  *)
(*   method say () = x  *)
(* end[@bs] *)
