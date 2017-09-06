type output = {
  stderr : string ; 
  stdout : string ;
  exit_code : int 
}


val perform : string -> string array -> output 


val perform_bsc : string array -> output 


 val bsc_check_eval : string -> output  

val debug_output : output -> unit 