let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc (x, y) = 
  incr test_id ; 
  suites := 
    (loc ^" id " ^ (string_of_int !test_id), (fun _ -> Mt.Eq(x,y))) :: !suites


[%%bs.raw{|
function hey(x, y) {
    if (x === void 0) { x = 3; }
    return x + y;
  }
|}
]
external xx : ?x:int -> y:int -> unit -> int  = "hey" [@@bs.val]

let u = xx ~y:3 ()

let z = xx ~x:(2 + 3 ) ~y:3 ()

let () = 
  eq __LOC__ ((u,z), (6,8))

let counter = ref 0 
let side_effect = fun [@bs] x -> incr x ; !x 
 
let bug_to_fix f x = 
  xx ~x:(f x [@bs]) ~y: 3 () (* : [f x] is done once *)

let bug_to_fix2 f x =
  xx ?x:(f x [@bs]) ~y: 3 () (* : [f x] is done once *)

let counter2 = ref 0 
let side_effect2 = fun [@bs] x -> incr x ; Some (!x )

let () = 
  let v = bug_to_fix side_effect counter in
  let pair = ((v, !counter), (4, 1)) in
  let v2 = bug_to_fix2 side_effect2 counter2 in 
  let pair2 = ((v2, !counter),(4,1)) in
  (* Js.log (pair,pair2) *)
  eq __LOC__ pair;
  eq __LOC__ pair2

[%%bs.raw{|
function heystr(x, y) {
    if (x === void 0) { x = "3"; }
    return x + y;
  }
  |}]

external kk : ?name:string -> string -> string = "heystr" [@@bs.val]

let () = 
  let pair = ("name4", kk ~name:"name" "4") in
  (* Js.log pair ; *)
  eq __LOC__ pair

;; Mt.from_pair_suites __MODULE__ !suites
