
type mode =
| Native
| Playground of string list (* 3rd party libraries folders paths *)

val from_cmj :
  mode: mode ->
  string list ->
  string ->
  unit


val from_cmi :
  string list ->
  string ->
  unit
