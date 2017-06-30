
let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = 
  incr test_id ; 
  suites := 
    (loc ^" id " ^ (string_of_int !test_id), (fun _ -> Mt.Eq(x,y))) :: !suites



type a = {
  b: b;
}
and b = {
  a: a
};;

let rec a = { b } and b = { a };;

let is_inifite (x : a) = 
  x.b.a == x




;; eq __LOC__ true (is_inifite a)


;; Mt.from_pair_suites __FILE__ !suites
