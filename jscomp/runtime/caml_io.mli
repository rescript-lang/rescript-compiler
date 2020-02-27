

(* not complaining for JS tools *)
val stdin : 'a Caml_undefined_extern.t (* [@@dead "stdin"] *)
type out_channel

val stdout : out_channel (* [@@dead "stdout"] *)

val stderr : out_channel (* [@@dead "stderr"] *)

val caml_ml_flush :  (* [@@dead "caml_ml_flush"] *)
  out_channel -> 
  unit 

val caml_ml_output :     (* [@@dead "caml_ml_output"] *)
  out_channel -> 
  string -> 
  int -> 
  int -> 
  unit 

val caml_ml_output_char :    (* [@@dead "caml_ml_output_char"] *)
  out_channel -> 
  char -> 
  unit 

val caml_ml_out_channels_list :    (* [@@dead "caml_ml_out_channels_list"] *)
  unit -> 
  out_channel list 
  