


external test_string_type : ([`on_closed | `on_open | `in_ [@bs.as "in"]]
                [@bs.string]) -> int  = 
  "hey_string" [@@bs.call]

external test_int_type : ([`on_closed | `on_open | `in_ [@bs.as "in"]]
                [@bs.int]) -> int  = 
  "hey_int" [@@bs.call]

val uu : int array
val vv : int array

type readline

val register : readline -> unit

val test : readline ->
  [ `close of (unit -> unit [@bs])
  | `line of (string -> unit [@bs]) ] -> unit
