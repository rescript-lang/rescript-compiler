type action =
    Print of string
  | Print_arg
  | Skip_arg;;

val find_macro: string -> action list;;

val def_macro: string -> action list -> unit;;
