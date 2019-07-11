let suites : Mt.pair_suites ref = ref []
let test_id = ref 0

let eq loc x y =
  incr test_id ;
  suites :=
    (loc ^ " id " ^ string_of_int !test_id, fun _ -> Mt.Eq (x, y)) :: !suites

[%%bs.raw
{|
function add(x,y){
  return x + y
}
|}]

type _ kind = Float : float kind | String : string kind

external add : ('a kind[@bs.ignore]) -> 'a -> 'a -> 'a = "add" [@@bs.val]

let v = ref 0

external config : hi:int -> lo:int -> unit -> _ = "" [@@bs.obj]

let h = config ~hi:2 ~lo:0 (ignore (incr v))
let z = add (incr v ; Float) 3.0 2.0
let () = eq __LOC__ !v 2 ; eq __LOC__ z 5.0
let () = Mt.from_pair_suites __MODULE__ !suites
