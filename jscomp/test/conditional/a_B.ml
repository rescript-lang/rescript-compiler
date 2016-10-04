module B
= struct
#1 "b.ml"
let value = 3


end
module A
= struct
#1 "a.ml"

let u =

    B.value


end
(* B=true bspack.exe -bs-main a.ml -o a_B.ml *)
