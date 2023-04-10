let v = "gso"

let suites = {
  open Mt
  list{
    (
      "equal",
      _ => Eq(
        (Bytes.get(Bytes.make(3, 'a'), 0), Bytes.unsafe_get(Bytes.make(3, 'a'), 0)),
        ('a', 'a'),
      ),
    ),
    (
      "equal2",
      _ => {
        let u = Bytes.make(3, 'a')
        Bytes.unsafe_set(u, 0, 'b')
        Eq((Bytes.unsafe_get(u, 0), String.get(v, 0)), ('b', 'g'))
      },
    ),
    (
      "buffer",
      _ => {
        let v = Buffer.create(30)
        for i in 0 to 10 {
          Buffer.add_string(v, string_of_int(i))
        }
        Eq(Buffer.contents(v), "012345678910")
      },
    ),
  }
}

Mt.from_pair_suites(__MODULE__, suites)
