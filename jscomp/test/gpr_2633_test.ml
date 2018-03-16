type foo

external on :
  foo ->
  ([`bar of unit -> unit
   | `foo of string -> unit ] [@bs.string]) ->
   unit = "" [@@bs.send]

let on1 foo event =
  on foo event

(* FIXME *)
let on2 foo h event =
  on foo (h event)

