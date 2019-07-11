let to_unsgined x = Nativeint.shift_right_logical x 0
let f (x : nativeint) = to_unsgined (to_unsgined (to_unsgined x))

(* TODO: this should be optimized as a single [to_unsigned]*)
let ff (x : nativeint) = x |> to_unsgined |> to_unsgined

open Nativeint

let fff x = add 3n @@ add 3n @@ add 4n @@ add 1n x
