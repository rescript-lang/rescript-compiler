(* data comparison utilities
 * compute and display difference between two pieces of data *)

(* represents a place where some lines are deleted and some are inserted *)
type change

(* the result of a comparison is a diff; a chain of changes *)
type diff = change list

val diff:
  onChange:(change -> unit)
  -> compareFn:('data -> 'data -> int)
  -> 'data array
  -> 'data array
  -> unit

(* compute and print line-based diff between two strings *)
val diffTwoStrings: string -> string -> unit
