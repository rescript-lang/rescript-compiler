val print: is_warning:bool -> src:string -> startPos:Lexing.position -> endPos:Lexing.position -> string

val setup : Misc.Color.setting option -> unit
(* [setup opt] will enable or disable color handling for print
   according to the value of color setting [opt].
   Only the first call to this function has an effect. *)
