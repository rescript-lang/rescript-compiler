(** {!Belt.Float}
    Utililites for Float
*)

external toInt: float -> int = "%intoffloat"

external fromInt: int -> float = "%identity"

external fromString: string -> float = "parseFloat" [@@bs.val]

external toString: float -> string = "String" [@@bs.val]

let (+) = (+.)

let (-) = (-.)

let (/) = (/.)

let ( * ) = ( *. )
