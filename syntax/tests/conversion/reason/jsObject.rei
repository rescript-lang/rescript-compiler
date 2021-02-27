type propField('a) = Js.t({.})
type propField('a) = Js.t({..} as 'a)
type propField('a) = Js.t({..}) as 'a
type propField('a) = Js.nullable(Js.t({..} as 'a))

type propField('a) = {. "a": b}
type propField('a) = {.. "a": b}
type propField('a) = Js.t(Js.t({. "a": Js.t({. "b": c})}))
