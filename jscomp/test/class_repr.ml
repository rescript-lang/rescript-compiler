

class x v = object 
  val x = v 
end

(* let () = [%bs.debug] *)
let v = new x 3
let u = Oo.copy v 
