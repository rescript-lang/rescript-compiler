(* Interface to print source code from different languages to res.
 * Takes a filename called "input" and returns the corresponding formatted res syntax *)
val print : [`ml | `res] -> input:string -> string
