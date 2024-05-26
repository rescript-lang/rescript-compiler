let dictCreationCanBeInlined = Js.Dict.fromArray([
  ("name", "hello"),
  ("age", "what"),
  ("more", "stuff"),
])

external imaginaryExternalArgs: array<(string, string)> = "imaginary"

let dictCreationCanNotBeInlined = Js.Dict.fromArray(imaginaryExternalArgs)

let dictCreationCanBeInlined2 = Js.Dict._unsafe_create([
  ("name", "hello"),
  ("age", "what"),
  ("more", "stuff"),
])
