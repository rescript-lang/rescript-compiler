type t<'a> = Js.Set.t<'a>

@new external make: unit => t<'a> = "Set"
@new external fromArray: array<'a> => t<'a> = "Set"
@new external fromIterator: Iterator.t<'a> => t<'a> = "Set"

@get external size: t<'a> => int = "size"

@send external clear: t<'a> => unit = "clear"

@send external add: (t<'a>, 'a) => unit = "add"
@send external delete: (t<'a>, 'a) => bool = "delete"
@send external has: (t<'a>, 'a) => bool = "has"

@send external forEach: (t<'a>, 'a => unit) => unit = "forEach"

@send external values: t<'a> => Iterator.t<'a> = "values"
