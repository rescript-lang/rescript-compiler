type _ eq = 
  | Eq :  'a *'a  -> _ eq
  | Neq : 'a * 'a -> _ eq
  | Approx : float * float -> _ eq 

type 'a pair_suites = (string * (unit -> 'a eq)) list

val from_suites : string -> (string * (unit -> unit)) list -> unit
val from_pair_suites : string -> 'a pair_suites -> unit



