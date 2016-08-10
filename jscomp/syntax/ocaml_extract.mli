type output =
  [ `All of
      string *
      string *
      string *
      string *
      string
  | `Ml of string * string * string
  | `Mli of string * string * string ]
type outputs  = output list  

val process_as_string : 
  string list ->   outputs


val process :
  string list -> outputs

