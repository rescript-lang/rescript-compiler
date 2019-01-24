let (.?[]) = Hashtbl.find_opt
let (.@[]) = Hashtbl.find
let ( .@[]<- ) = Hashtbl.add
let (.@{}) = Hashtbl.find
let ( .@{}<- ) = Hashtbl.add
let (.@()) = Hashtbl.find
let ( .@()<- ) = Hashtbl.add

let h = Hashtbl.create 17

;;
  h.@("One") <- 1
; assert (h.@{"One"} = 1)
; print_int h.@{"One"}
; assert (h.?["Two"] = None)


(* from GPR#1392 *)
let ( #? ) x y = (x, y);;
let ( .%() ) x y = x.(y);;
let x = [| 0 |];;
let _ = 1 #? x.(0);;
let _ = 1 #? x.%(0);;
