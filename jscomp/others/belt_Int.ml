(** {!Belt.Int}
    Utililites for Int
*)

external toFloat: int -> float = "%identity"

external fromFloat: float -> int = "%intoffloat"

external fromString: string -> (_ [@bs.as 10]) -> int = "parseInt" [@@bs.val]

external toString: int -> string = "String" [@@bs.val]

let (+) = (+)

let (-) = (-)

let (/) = (/)

let ( * ) = ( * )
