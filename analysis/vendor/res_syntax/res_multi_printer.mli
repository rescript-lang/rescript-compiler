(* Interface to print source code from different languages to res.
 * Takes a filename called "input" and returns the corresponding formatted res syntax *)
val print : ?ignore_parse_errors:bool -> [`ml | `res] -> input:string -> string
