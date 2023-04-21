let count = 1_000_000

module N = Belt.Set.Int
let bench = () => {
  let data = ref(N.empty)
  %time(
    for i in 0 to count {
      data := N.add(data.contents, i)
    }
  )
  %time(
    for i in 0 to count {
      assert(N.has(data.contents, i))
    }
  )
  %time(
    for i in 0 to count {
      data := N.remove(data.contents, i)
    }
  )
  assert (N.size(data.contents) == 0)
}

%time(bench())
