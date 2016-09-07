module A0_a1
= struct
#1 "a0_a1.ml"
let v  = 3

end
module A1_a2 : sig 
#1 "a1_a2.mli"
val v : int 

end = struct
#1 "a1_a2.ml"
let v = A0_a1.v

end
module A2_a3
= struct
#1 "a2_a3.ml"
let v = A1_a2.v

end
module A3_a4
= struct
#1 "a3_a4.ml"
include A2_a3

end
module A4_a5
= struct
#1 "a4_a5.ml"
include A3_a4

;; Js.log v 

end
