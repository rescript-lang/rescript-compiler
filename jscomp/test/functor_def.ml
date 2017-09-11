
let v = ref 0

let f x x = 
  incr v ; 
  x + x 

let return () = !v 

module Make (U  : sig
  type t 
  val say : int -> int -> int 
  end ) = struct 
    (* let () = Js.log "no inline" *)
    let h x x = 
      Js.log (f x x);
      U.say x x

end    
