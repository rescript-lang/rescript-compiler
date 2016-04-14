type  eq = Mt.eq = 
  | Eq :  'a *'a  ->  eq
  | Neq : 'a * 'a ->  eq
  | Approx : float * float ->  eq  
  | ThrowAny : (unit -> unit) ->  eq
type  pair_suites = (string * (unit ->  eq)) list


let from_pair_suites (name : string) (suites :  pair_suites) = 
  Js.log (name, "testing");
  List.iter (fun (name, code) -> 
              match code () with 
              | Eq(a,b) -> Js.log (name , a, "eq?", b )
              | Neq(a,b) -> Js.log (name, a, "neq?",   b )
              | Approx(a,b) -> Js.log (name, a, "~",  b) 
              | ThrowAny fn -> ()
              ) suites

