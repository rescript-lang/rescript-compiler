

type dep_info = {
  dir_or_file : string ;
  stamp : float 
}



(** 
   The data structure we decided to whether regenerate [build.ninja] 
   or not. Note that if we don't record absolute path, 

   ninja will not notice  its build spec changed, it will not trigger 
   rebuild behavior, is this a desired behavior not?

   It may not, since there is some subtlies here (__FILE__ or __dirname)
*)
type t = 
  { file_stamps : dep_info array ; 
    source_directory :  string
  }





val write : string -> t -> unit



(** check if [build.ninja] should be regenerated *)
val check : cwd:string ->  string -> string
