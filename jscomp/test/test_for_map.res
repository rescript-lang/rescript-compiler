module IntMap = Map.Make({
  type t = int
  let compare = (x: int, y) => compare(x, y)
})

let assertion_test = () => {
  let m = ref(IntMap.empty)
  let count = 1000000
  for i in 0 to count {
    m := IntMap.add(i, i, m.contents)
  }
  for i in 0 to count {
    ignore(IntMap.find(i, m.contents))
  }
}
