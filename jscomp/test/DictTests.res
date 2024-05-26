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

open Js

let dictCreationCanBeInlined3 = Dict.fromArray([
  ("name", "hello"),
  ("age", "what"),
  ("more", "stuff"),
])

open Dict

let dictCreationCanBeInlined4 = fromArray([("name", "hello"), ("age", "what"), ("more", "stuff")])
