@@warning("-107")
let hash_variant = s => {
  let accu = ref(0)
  for i in 0 to String.length(s) - 1 {
    accu := land(223 * accu.contents + Char.code(String.get(s, i)), lsl(1, 31) - 1)
    /* Here accu is 31 bits, times 223 will not be than 53 bits..
       TODO: we can use `Sys.backend_type` for patching
 */
  }

  /* reduce to 31 bits */
  /* accu := !accu land (1 lsl 31 - 1); */
  /* make it signed for 64 bits architectures */
  if accu.contents > 0x3FFFFFFF {
    lor(accu.contents - lsl(1, 31), 0)
  } else {
    accu.contents
  }
}

let hash_variant2 = s => {
  let accu = ref(0)
  for i in 0 to String.length(s) - 1 {
    accu := 223 * accu.contents + Char.code(String.get(s, i))
  }
  /* reduce to 31 bits */
  accu := land(accu.contents, lsl(1, 31) - 1)

  /* make it signed for 64 bits architectures */
  if accu.contents > 0x3FFFFFFF {
    accu.contents - lsl(1, 31)
  } else {
    accu.contents
  }
}

let rec fib = x =>
  switch x {
  | 0l | 1l => 1l
  | n => Int32.add(fib(Int32.sub(n, 1l)), fib(Int32.sub(n, 2l)))
  }

Mt.from_pair_suites(
  __MODULE__,
  list{
    (
      "plus_overflow",
      _ => Eq(
        true,
        {
          open Int32
          add(max_int, 1l)
        } == Int32.min_int,
      ),
    ),
    (
      "minus_overflow",
      _ => Eq(
        true,
        {
          open Int32
          sub(min_int, one)
        } == Int32.max_int,
      ),
    ),
    (
      "flow_again",
      _ => Eq(
        2147483646l,
        {
          open Int32
          add(add(max_int, max_int), min_int)
        },
      ),
    ),
    (
      "flow_again",
      _ => Eq(
        -2l,
        {
          open Int32
          add(max_int, max_int)
        },
      ),
    ),
    ("hash_test", _ => Eq(hash_variant("xxyyzzuuxxzzyy00112233"), 544087776)),
    ("hash_test2", _ => Eq(hash_variant("xxyyzxzzyy"), -449896130)),
    (__LOC__, _ => Eq(hash_variant2("xxyyzzuuxxzzyy00112233"), 544087776)),
    (__LOC__, _ => Eq(hash_variant2("xxyyzxzzyy"), -449896130)),
    ("int_literal_flow", _ => Eq(-1, 0xffffffff)),
    ("int_literal_flow2", _ => Eq(-1l, Int32.of_int(0xffffffff))),
    ("int_literal_flow3", _ => Eq(-1l, Int32.of_int(0xfffffffff))),
    ("int32_mul", _ => Eq(-33554431l, Int32.mul(0xffffffl, 0xffffffl))),
    (__LOC__, _ => Eq(\"@@"(int_of_float, Js.Float.fromString("3")), 3)),
    /* FIXME */
    (__LOC__, _ => Eq(\"@@"(int_of_float, Js.Float.fromString("3.2")), 3)),
  },
)
