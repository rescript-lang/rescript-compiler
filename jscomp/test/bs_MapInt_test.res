@@bs.config({flags: ["-bs-no-cross-module-opt"]})
let should = b =>
  if !b {
    Js.Exn.raiseError("IMPOSSIBLE")
  }

module M = Belt.Map.Int
let test = () => {
  let m = ref(M.empty)
  let count = 100_0000 - 1
  for i in 0 to count {
    m := M.set(m.contents, i, i)
  }
  for i in 0 to count {
    should(M.get(m.contents, i) != None)
  }
  for i in 0 to count {
    m := M.remove(m.contents, i)
  }
  should(M.isEmpty(m.contents))
}

let () = test()
