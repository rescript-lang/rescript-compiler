

val error_of_printer :
  Location.t -> 
  (Format.formatter -> 'a -> unit) -> 
  'a -> 
  Location.error

val error_of_printer_file :
  (Format.formatter -> 'a -> unit) -> 
  'a -> 
  Location.error

  
val setup : unit -> unit
