type t

val toString : t -> string

val loc : t -> Location.t
val txt : t -> string
val prevTokEndPos : t -> Lexing.position

val setPrevTokEndPos : t -> Lexing.position -> unit

val isDocComment : t -> bool

val isModuleComment : t -> bool

val isSingleLineComment : t -> bool

val makeSingleLineComment : loc:Location.t -> string -> t
val makeMultiLineComment :
  loc:Location.t -> docComment:bool -> standalone:bool -> string -> t
val fromOcamlComment :
  loc:Location.t -> txt:string -> prevTokEndPos:Lexing.position -> t
val trimSpaces : string -> string
