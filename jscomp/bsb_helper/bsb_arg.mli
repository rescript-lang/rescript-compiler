


type spec =
  | Unit of (unit -> unit)       
  | Set of bool ref            
  | String of (string -> unit) 
  | Set_string of string ref   
  | Int of (int -> unit)       
  | Set_int of int ref         

type key = string
type doc = string
type usage_msg = string
type anon_fun = (string -> unit)

val parse_exn :
  (key * spec * doc) list -> anon_fun -> usage_msg -> unit



