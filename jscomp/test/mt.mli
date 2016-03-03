type + _ eq = 
  | Eq :  'a *'a  -> 'b eq
  | Neq : 'a * 'a -> 'c eq
  | Approx : float * float -> 'd eq 

type 'a pair_suites = (string * (unit -> 'a eq)) list

val from_suites : string -> (string * (unit -> unit)) list -> unit
val from_pair_suites : string -> 'a pair_suites -> unit



