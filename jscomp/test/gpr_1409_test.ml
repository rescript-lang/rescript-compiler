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

;; eq __LOC__ (b_##foo) (Js.Undefined.return "42" )

;; Js.log (Js.Obj.keys a_)
;; Js.log4 a b a_ b_


;; eq __LOC__ (Array.length (Js.Obj.keys a_)) 0


external mangle : ?_open:int -> ?xx__hi:int -> hi:int -> unit -> _ = ""
[@@bs.obj]


let test2 = mangle ~hi:2 ()

let test3 _open xx__hi = 
  Js.log "no inlin";
  mangle ?_open ?xx__hi ~hi:2 ()

let test4 _open xx__hi = 
  Js.log "no inlin";
  mangle ?_open:(Some _open) ?xx__hi ~hi:2 ()


let test5 f x = 
  Js.log "no inline";
  mangle ?_open:(f x ) ?xx__hi:(f x ) ~hi:2 ()

let test6 f x = 
  Js.log "no inline";
  let x = ref 3 in 
  mangle ?_open:(incr x ; Some !x) ?xx__hi:(f x [@bs]) ~hi:2 ()


let keys xs ys = 
  String_set.equal 
  (String_set.of_list xs)
  (String_set.of_list (Array.to_list ys))

;; eq __LOC__ (keys ["hi"] (Js.Obj.keys (test3 None None))) true

;; eq __LOC__ (keys ["hi"; "open"] (Js.Obj.keys (test3 (Some 2) None))) true

;; eq __LOC__ (keys ["hi"; "open"; "xx"] (Js.Obj.keys (test3 (Some 2) (Some 2) ) )) true

;; Mt.from_pair_suites __MODULE__ !suites