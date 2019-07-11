type state = Empty | Int1 of int | Int2 of int

let func (state : state) =
  ( match state with
    | Empty -> 0
    | Int2 intValue | Int1 intValue ->
        let intFunc intParam = intParam + intValue in
        intFunc 0
    : int )
