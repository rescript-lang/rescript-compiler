let count = 1_000_000
let bench = () => {
  let table = Hashtbl.create(1_000_000)
  for i in 0 to count {
    Hashtbl.add(table, i, i)
  }
  for i in 0 to count {
    assert(Hashtbl.mem(table, i))
  }
  for i in 0 to count {
    Hashtbl.remove(table, i)
  }
}

bench()
