type 'a eq = 'a Mt.eq = 
  | Eq :  'a *'a  -> _ eq
  | Neq : 'a * 'a -> _ eq
  | Approx : float * float -> _ eq  
  | ThrowAny : (unit -> unit) -> _ eq
type 'a pair_suites = (string * (unit -> 'a eq)) list


let from_pair_suites (name : string) (suites : 'a pair_suites) = 
  Js.log (name, "testing");
  List.iter (fun (name, code) -> 
              match code () with 
              | Eq(a,b) -> Js.log (name , a, "eq?", b )
              | Neq(a,b) -> Js.log (name, a, "neq?",   b )
              | Approx(a,b) -> Js.log (name, a, "~",  b) 
              | ThrowAny fn -> ()
              ) suites

