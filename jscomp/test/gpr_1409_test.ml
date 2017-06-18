let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = 
  incr test_id ; 
  suites := 
    (loc ^" id " ^ (string_of_int !test_id), (fun _ -> Mt.Eq(x,y))) :: !suites




(*type t*)
external make : ?foo:string -> unit -> _ = "" [@@bs.obj]


let a = make ()
let b = make ~foo:"42" ()

let map f x = 
  match x with
  | None -> None 
  | Some x -> Some (f x)

let make ?foo:(foo: int option) = 
	make ?foo:(map string_of_int foo)


let a_ = make ()
let b_ = make ~foo:42 ()  

;; Js.log (Js.Obj.keys a_)
;; Js.log4 a b a_ b_


;; eq __LOC__ (Array.length (Js.Obj.keys a_)) 0
;; Mt.from_pair_suites __FILE__ !suites