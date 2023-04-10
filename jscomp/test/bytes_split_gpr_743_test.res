let suites: ref<Mt.pair_suites> = ref(list{})
let test_id = ref(0)
let eq = (loc, (x, y)) => {
  incr(test_id)
  suites :=
    list{(loc ++ (" id " ++ string_of_int(test_id.contents)), _ => Mt.Eq(x, y)), ...suites.contents}
}

let () = {
  let b = Bytes.create(3)
  Bytes.set(b, 0, 'a')
  Bytes.set(b, 1, 'b')
  Bytes.set(b, 2, 'c')
  Bytes.blit(b, 0, b, 1, 2)
  let res = Bytes.unsafe_to_string(b)
  Js.log(res) /* "aab" */
  eq(__LOC__, ("aab", res))
}
/* assert('a' = Bytes.get b 0); */
/* assert('a' = Bytes.get b 1); */
/* assert('b' = Bytes.get b 2); */
/* () */

let () = {
  let b = Bytes.create(3)
  Bytes.set(b, 0, 'a')
  Bytes.set(b, 1, 'b')
  Bytes.set(b, 2, 'c')
  Bytes.blit(b, 1, b, 0, 2)
  let res2 = Bytes.unsafe_to_string(b)
  Js.log(res2) /* "bcc" */
  eq(__LOC__, ("bcc", res2))
}

let () = Mt.from_pair_suites(__MODULE__, suites.contents)
