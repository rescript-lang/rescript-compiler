(** Range coordinates all 1-indexed, like for editors. Otherwise this code
  would have way too many off-by-one errors *)
val print_file: is_warning:bool -> range:(int * int) * (int * int) -> lines:string array -> Format.formatter -> unit -> unit
