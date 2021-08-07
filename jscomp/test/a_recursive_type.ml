type t = A of (t -> int)

let g = fun  x -> 
  match x with 
  | A v -> v x

let loop = g (A g ) 

let non_terminate = (fun  x -> 
  match x with 
  | A v -> v x) (A g)

  type t0 = { xx : t0 } 
  (* [@@unboxed]*)

  let rec xx = { xx }

type t1 = 
   | A of t1 array
   


type t2 = 
   | A2 of t2 array [@@unboxed]

(* let rec h = A [|h|]     
let rec h1 = A [|h1|] // could be relaxed
let rec h2 = A2 [|h2|]  

;; Js.log (h,h2) *)
(** If we inline g's definition -- it will be the same, inline uncarefully 
    (inline the inlined result)
    will make it non-terminating
 *)
