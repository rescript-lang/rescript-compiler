

(* not complaining for JS tools *)
val stdin : 'a Caml_undefined_extern.t
type out_channel

val stdout : out_channel

val stderr : out_channel

val caml_ml_flush : 
  out_channel -> 
  unit 

val caml_ml_output :    
  out_channel -> 
  string -> 
  int -> 
  int -> 
  unit 

val caml_ml_output_char :   
  out_channel -> 
  char -> 
  unit 

val caml_ml_out_channels_list :   
  unit -> 
  out_channel list 
  