(* utilities to read and write to/from files or stdin *)

(* reads the contents of "filename" into a string *)
val readFile: filename: string -> string

(* read the contents of stdin into a string*)
val readStdin: unit -> string

(* writes "content" into file with name "filename" *)
val writeFile: filename: string -> contents: string -> unit
