open RescriptCore

let eq = (a, b) => a == b

Test.run(__POS_OF__("make"), Dict.make(), eq, %raw(`{}`))

Test.run(__POS_OF__("fromArray"), Dict.fromArray([("foo", "bar")]), eq, %raw(`{foo: "bar"}`))

Test.run(
  __POS_OF__("getUnsafe - existing"),
  Dict.fromArray([("foo", "bar")])->Dict.getUnsafe("foo"),
  eq,
  "bar",
)
Test.run(
  __POS_OF__("getUnsafe - missing"),
  Dict.make()->Dict.getUnsafe("foo"),
  eq,
  %raw(`undefined`),
)
