let count = 1_000_000

module V = Rbset
let bench = () => {
  let data = ref(V.empty)
  %time(
    for i in 0 to count {
      data := V.add(i, data.contents)
    }
  )
  %time(
    for i in 0 to count {
      assert(V.mem(i, data.contents))
    }
  )
  %time(
    for i in 0 to count {
      data := V.remove(i, data.contents)
    }
  )
  assert (V.cardinal(data.contents) == 0)
}

%time(bench())
