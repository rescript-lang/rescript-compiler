let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = 
  incr test_id ; 
  suites := 
    (loc ^" id " ^ (string_of_int !test_id), (fun _ -> Mt.Eq(x,y))) :: !suites



type element
type dom
external getElementById : string -> element option = ""
[@@bs.send.pipe:dom] [@@bs.return nullable] 

let test dom =
    let elem = dom |> getElementById "haha" in
    match elem with
    | None -> 1
    | Some ui -> Js.log ui ; 2
    
    
let f x y = 
  Js.log "no inline";
  Js.Nullable.return (x + y)

;; eq  __LOC__ (Js.test (Js.Nullable.return 3 )) false

;; eq  __LOC__ (Js.test ((f 1 2) )) false

;; eq __LOC__ (Js.test [%raw "null"]) true 

;; let null2 = Js.Nullable.return 3 in 
let null = null2 in 
eq __LOC__ (Js.test null) false
;; Mt.from_pair_suites __FILE__ !suites