type  eq = 
  | Eq :  'a *'a  ->  eq
  | Neq : 'a * 'a ->  eq
  | Approx : float * float ->  eq 
  | ThrowAny : (unit -> unit) -> eq
type  pair_suites = (string * (unit ->  eq)) list

val from_suites : string -> (string * (unit -> unit)) list -> unit
val from_pair_suites : string ->  pair_suites -> unit




