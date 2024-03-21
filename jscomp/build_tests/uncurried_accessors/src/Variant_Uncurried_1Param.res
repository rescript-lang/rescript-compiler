@@uncurried
@deriving(accessors)
type variant = | Num(int) 

//Asserts the correct signature for derived accessor
let _numAlias: int => variant = num

//Asserts that inference works when composing
//with derived functions
let compose = (a, accessor) => accessor(a)
let _composedNum = compose(1, num)
