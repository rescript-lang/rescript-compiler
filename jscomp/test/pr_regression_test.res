let a = {
  let v = ref(3)
  let action = () => incr(v)
  let f = h =>
    z => ((x, y) => h(x, y))({
      action()
      3
    })(z)
  \"@@"(ignore, f(\"+"))
  \"@@"(ignore, f(\"+"))
  v.contents
}

let b = {
  let v = ref(3)
  let action = () => incr(v)
  let f = h =>
    z => ((x, y) => h(x, y))({
      action()
      3
    })(z)
  \"@@"(ignore, f(\"+"))
  \"@@"(ignore, f(\"+"))
  v.contents
}

let c = {
  let v = ref(3)
  let action = () => incr(v)
  let f = h =>
    z => ((x, y) => h(x, y))(
      2,
      {
        action()
        3
      },
    )(z)
  \"@@"(ignore, f(\"+"))
  \"@@"(ignore, f(\"+"))
  v.contents
}

let d = {
  let v = ref(3)
  let action = () => incr(v)
  let f = (h, g) =>
    z => ((x, y) => h(x, y))({
      let v = 3
      action()
      v * v
    })(z)
  \"@@"(ignore, f(\"+", 3))
  \"@@"(ignore, f(\"+", 3))
  v.contents
}

Mt.from_pair_suites(
  __MODULE__,
  {
    open Mt
    list{("partial", _ => Eq((5, 5, 5, 5), (a, b, c, d)))}
  },
)
