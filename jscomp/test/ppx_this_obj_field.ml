

let v5  =
  object(self)
    val  x = 3
    val mutable y = 3
    method private setY v  =  
      self##y #= 2 ;
      self##y, v
    method say () =
      self##x + self##y
    method private hihi u = 
      self##x + self##say ()
    method private bark () = 
      Js.log "bark"
    method xz () =  3
  end [@bs]

let () = 
  Js.log (v5##say ())
(* let v6 x = object  *)
(*   method say () = x  *)
(* end[@bs] *)
