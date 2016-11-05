

type dep_info = {
  dir_or_file : string ;
  stamp : float 
}

type t = dep_info array 




val write : string -> t -> unit



(** check if [build.ninja] should be regenerated *)
val check : string -> string
