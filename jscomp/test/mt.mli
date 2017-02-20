type  eq =
  | Eq :  'a *'a  ->  eq
  | Neq : 'a * 'a ->  eq
  | Ok : bool -> eq
  | Approx : float * float ->  eq
  | ApproxThreshold : float * float * float ->  eq
  | ThrowAny : (unit -> unit) -> eq
type  pair_suites = (string * (unit ->  eq)) list

val from_suites : string -> (string * (unit -> unit)) list -> unit
val from_pair_suites : string ->  pair_suites -> unit
