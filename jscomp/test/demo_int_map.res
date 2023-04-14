module IntMap = Map.Make({
  type t = int
  let compare = (x: int, y) => x - y
})

let test = () => {
  let m = ref(IntMap.empty)
  let count = 1000_000
  for i in 0 to count {
    m := IntMap.add(i, i, m.contents)
  }
  for i in 0 to count {
    ignore(IntMap.find(i, m.contents))
  }
}

let () = /* Js.log "start" ; */
test()
/* Js.log "finish" */
