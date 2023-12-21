let dictCreationCanBeInlined = Js.Dict.fromArray([
  ("name", "hello"),
  ("age", "what"),
  ("more", "stuff"),
])

external imaginaryExternalArgs: array<(string, string)> = "imaginary"

let dictCreationCanNotBeInlined = Js.Dict.fromArray(imaginaryExternalArgs)
