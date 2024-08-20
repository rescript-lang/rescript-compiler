@@config({flags: ["-bs-no-cross-module-opt"]})

open Node_test

let ok = (loc, a) => Node_assert.ok(a, ~message=loc)

module M = Belt.Map.Int

describe("Belt.Map.Int", () => {
  test("set", () => {
    let m = ref(M.empty)
    let count = 100_0000 - 1

    for i in 0 to count {
      m := M.set(m.contents, i, i)
    }
    for i in 0 to count {
      ok(__LOC__, M.get(m.contents, i) != None)
    }
    for i in 0 to count {
      m := M.remove(m.contents, i)
    }

    ok(__LOC__, M.isEmpty(m.contents))
  })
})
