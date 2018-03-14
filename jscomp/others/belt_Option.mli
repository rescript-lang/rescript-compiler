val getExn : 'a option -> 'a
val foldU : 'a option -> 'b -> ('a -> 'b [@bs]) -> 'b
val fold : 'a option -> 'b -> ('a -> 'b) -> 'b
val mapU : 'a option -> ('a -> 'b [@bs]) -> 'b option
val map : 'a option -> ('a -> 'b) -> 'b option
val flatMapU : 'a option -> ('a -> 'b option [@bs]) -> 'b option
val flatMap : 'a option -> ('a -> 'b option) -> 'b option
val getOrElse : 'a option -> 'a -> 'a
val has : 'a option -> bool
val isEmpty : 'a option -> bool
