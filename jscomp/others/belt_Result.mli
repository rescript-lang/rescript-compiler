type ('a, 'b) t =
  | Ok of 'a
  | Error of 'b

val ok : 'a -> ('a, 'b) t
val error : 'b -> ('a, 'b) t

val result : ('a -> 'c) -> ('b -> 'c) -> ('a, 'b) t -> 'c

val map : ('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t
val (<$>) : ('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t
val mapLeft : ('a -> 'c) -> ('a, 'b) t -> ('c, 'b) t

val bimap : ('a -> 'c) -> ('b -> 'd) -> ('a, 'b) t -> ('c, 'd) t

val isOk : ('a, 'b) t -> bool
val isError : ('a, 'b) t -> bool

val toString : ('a -> string) -> ('b -> string) -> ('a, 'b) t -> string

val fold : ('b -> 'c -> 'c) -> 'c -> ('a, 'b) t -> 'c

val oks: (('a, 'b) t) list -> 'a list
val errors: (('a, 'b) t) list -> 'b list
val arrayErrors: (('a, 'b) t) array -> 'a array
val arrayOks: (('a, 'b) t) array -> 'b array
