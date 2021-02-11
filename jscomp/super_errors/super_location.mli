
(* Needed for the online playground experience *)
val super_warning_printer :
  Warnings.loc ->
  Format.formatter ->
  Warnings.t -> unit

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
