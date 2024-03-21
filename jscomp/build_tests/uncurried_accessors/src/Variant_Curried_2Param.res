@deriving(accessors)
type variant = | DoubleNum(int, int)

//Asserts the correct signature for derived accessor
let _numAlias: (int, int) => variant = doubleNum

//Asserts that inference works when composing
//with derived functions
let compose = (a, b, accessor) => accessor(a, b)
let _composedNum = compose(1, 2, doubleNum)

