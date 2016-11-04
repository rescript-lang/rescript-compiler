

type dep_info = {
  dir_or_file : string ;
  stamp : float 
}

type t = dep_info array 


val check : string -> string

val write : string -> t -> unit
