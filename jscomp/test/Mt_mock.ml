type _ eq = 
  | Eq :  'a *'a  -> _ eq
  | Neq : 'a * 'a -> _ eq
  | Approx : float * float -> _ eq  

type 'a pair_suites = (string * (unit -> 'a eq)) list


let from_pair_suites name (suites : 'a pair_suites) = 
    List.iter (fun (name, code) -> 
              match code () with 
              | Eq(a,b) -> Js.log (name , a, "eq?", b )
              | Neq(a,b) -> Js.log (name, a, "neq?",   b )
              | Approx(a,b) -> Js.log (name, a, "~",  b) 
              ) suites

