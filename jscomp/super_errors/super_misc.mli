(** Range coordinates all 1-indexed, like for editors. Otherwise this code
  would have way too many off-by-one errors *)
val print_file: range:(int * int) * (int * int) -> lines:string array -> Format.formatter -> unit -> unit

(** This will be called in super_main. This is how you override the default error and warning printers *)
val colorize_tagged_string: Format.formatter -> (string -> Ext_color.style list) -> unit
