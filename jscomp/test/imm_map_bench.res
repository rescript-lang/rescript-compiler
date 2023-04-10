type map
@new @module("immutable") external empty: unit => map = "OrderedMap"
@send external set: (map, int, int) => map = "set"
@send @return(undefined_to_opt) external get: (map, int) => option<int> = "get"
@send external mem: (map, int) => bool = "has"

module A = Belt.Array
let empty = empty()
let fromArray = kvs => {
  let v = ref(empty)
  for i in 0 to A.length(kvs) - 1 {
    let (key, value) = A.getUnsafe(kvs, i)
    v := set(v.contents, key, value)
  }
  v.contents
}

let should = b =>
  if !b {
    Js.Exn.raiseError("impossible")
  }

let count = 1_000_000

let shuffledDataAdd = A.makeByAndShuffle(count + 1, i => (i, i))

let test = () => {
  let v = fromArray(shuffledDataAdd)
  for j in 0 to count {
    should(mem(v, j))
  }
}

module M = Belt.Map.Int

let test2 = () => {
  let v = M.fromArray(shuffledDataAdd)
  for j in 0 to count {
    should(M.has(v, j))
  }
}

%time(test())
%time(test2())
