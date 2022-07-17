type t

val initial : t
val exportEarly : emitters:t -> string -> t
val requireEarly : emitters:t -> string -> t
val export : emitters:t -> string -> t
val import : emitters:t -> string -> t
val require : emitters:t -> string -> t
val toString : separator:string -> t -> string
