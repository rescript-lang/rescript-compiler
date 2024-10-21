open RescriptCore

let eq = (a, b) => a == b

// =======
// forEach
// =======

let forEachIfOkCallFunction = () => {
  let called = ref([])
  Ok(3)->Result.forEach(i => called.contents->Array.push(i))
  Test.run(__POS_OF__("forEach: if ok, call function with ok value once"), called.contents, eq, [3])
}
forEachIfOkCallFunction()

let forEachIfErrorDoNotCallFunction = () => {
  let called = ref([])
  Error(3)->Result.forEach(i => called.contents->Array.push(i))
  Test.run(__POS_OF__("forEach: if error, do not call function"), called.contents, eq, [])
}
forEachIfErrorDoNotCallFunction()

// ========
// mapError
// ========

Test.run(__POS_OF__("mapError: if ok, return it"), Ok(5)->Result.mapError(i => i * 3), eq, Ok(5))

Test.run(
  __POS_OF__("mapError: if error, apply f"),
  Error(5)->Result.mapError(i => i * 3),
  eq,
  Error(15),
)
