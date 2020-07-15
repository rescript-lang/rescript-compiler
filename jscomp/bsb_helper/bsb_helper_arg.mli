


type string_action = 
  | Dummy  
  | Optional_set of 
    string option ref
type spec =
  | Bool of bool ref            
  | String of string_action 


type anon_fun = rev_args:string list -> unit

val parse_exn :
  progname:string -> 
  argv:string array -> 
  start:int ->
  (string * spec * string) array -> 
  anon_fun  -> unit



