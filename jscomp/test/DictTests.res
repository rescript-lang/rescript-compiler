let dictCreationCanBeInlined = Js.Dict.fromArray([
  ("name", "hello"),
  ("age", "what"),
  ("more", "stuff"),
])

external imaginaryExternalArgs: array<(string, string)> = "imaginary"

let dictCreationCanNotBeInlined = Js.Dict.fromArray(imaginaryExternalArgs)

let dictWithSyntax = dict{
  "first": 1,
  "second": 2,
  "third": 3
}