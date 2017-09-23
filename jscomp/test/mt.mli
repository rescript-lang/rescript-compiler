type  eq =
  | Eq :  'a *'a  ->  eq
  | Neq : 'a * 'a ->  eq
  | StrictEq :  'a *'a  ->  eq
  | StrictNeq : 'a * 'a ->  eq
  | Ok : bool -> eq
  | Approx : float * float ->  eq
  | ApproxThreshold : float * float * float ->  eq
  | ThrowAny : (unit -> unit) -> eq
  | Fail : unit -> eq
  | FailWith : string -> eq
type  pair_suites = (string * (unit ->  eq)) list

val from_suites : string -> (string * (unit -> unit)) list -> unit
val from_pair_suites : string ->  pair_suites -> unit

type promise_suites = (string * eq Js.Promise.t) list 

val from_promise_suites : 
  string ->
  promise_suites -> 
  unit