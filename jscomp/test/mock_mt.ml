type eq = Mt.eq =
  | Eq : 'a * 'a -> eq
  | Neq : 'a * 'a -> eq
  | StrictEq : 'a * 'a -> eq
  | StrictNeq : 'a * 'a -> eq
  | Ok : bool -> eq
  | Approx : float * float -> eq
  | ApproxThreshold : float * float * float -> eq
  | ThrowAny : (unit -> unit) -> eq
  | Fail : unit -> eq
  | FailWith : string -> eq

type pair_suites = (string * (unit -> eq)) list

let from_pair_suites (name : string) (suites : pair_suites) =
  Js.log (name, "testing") ;
  List.iter
    (fun (name, code) ->
      match code () with
      | Eq (a, b) -> Js.log (name, a, "eq?", b)
      | Neq (a, b) -> Js.log (name, a, "neq?", b)
      | StrictEq (a, b) -> Js.log (name, a, "strict_eq?", b)
      | StrictNeq (a, b) -> Js.log (name, a, "strict_neq?", b)
      | Approx (a, b) -> Js.log (name, a, "~", b)
      | ApproxThreshold (t, a, b) -> Js.log (name, a, "~", b, " (", t, ")")
      | ThrowAny fn -> ()
      | Fail _ -> Js.log "failed"
      | FailWith msg -> Js.log ("failed: " ^ msg)
      | Ok a -> Js.log (name, a, "ok?"))
    suites
