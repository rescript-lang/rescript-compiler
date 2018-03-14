val getExn : 'a option -> 'a
val fold : 'a option -> 'b -> ('a -> 'b) -> 'b
val map : 'a option -> ('a -> 'b) -> 'b option
val flatMap : 'a option -> ('a -> 'b option) -> 'b option
val getOrElse : 'a option -> 'a -> 'a
val has : 'a option -> bool
val isEmpty : 'a option -> bool
