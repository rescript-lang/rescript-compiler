@@uncurried
@deriving(accessors)
type myRecord = { myField: int }

//Asserts the correct signature for derived accessor
let _myFieldAlias: myRecord => int = myField

//Asserts that inference works when composing
//with derived functions
let compose = (a, accessor) => accessor(a)
let _composedNum = compose({ myField: 1 }, myField)
