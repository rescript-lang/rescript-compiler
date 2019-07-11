type t = A of (t -> int)

let g x = match x with A v -> v x
let loop = g (A g)
let non_terminate = (fun x -> match x with A v -> v x) (A g)

(** If we inline g's definition -- it will be the same, inline uncarefully
    (inline the inlined result) will make it non-terminating *)
