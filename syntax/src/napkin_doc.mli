type t

val nil: t
val line: t
val hardLine: t
val softLine: t
val text: string -> t
val concat: t list -> t
val indent: t -> t
val ifBreaks: t -> t -> t
val lineSuffix: t -> t
val group: t -> t
val breakableGroup: forceBreak : bool -> t -> t
val customLayout: t list -> t
val breakParent: t
val join: sep: t -> t list -> t

val space: t
val comma: t
val dot: t
val dotdot: t
val dotdotdot: t
val lessThan: t
val greaterThan: t
val lbrace: t
val rbrace: t
val lparen: t
val rparen: t
val lbracket: t
val rbracket: t
val question: t
val tilde: t
val equal: t
val trailingComma: t
val doubleQuote: t

val toString: width: int -> t -> string
val debug: t -> unit [@@live]