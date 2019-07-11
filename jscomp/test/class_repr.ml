class x v =
  object
    val x = v
  end

(* let () = [%bs.debugger] *)
let v = new x 3
let u = Oo.copy v
