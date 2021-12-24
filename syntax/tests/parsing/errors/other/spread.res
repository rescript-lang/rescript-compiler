let arr = [...x, ...y]
let [...arr, _] = [1, 2, 3]

let record = {...x, ...y}
let {...x, ...y} = myRecord

let myList = list{...x, ...y}
let list{...x, ...y} = myList

type t = {...a}
type t = Foo({...a})
type t = option<foo, {...x}>
