let dictCreationCanBeInlined = dict{
  "name": "hello",
  "age": "what",
  "more": "stuff",
}

external imaginaryExternalArgs: array<(string, string)> = "imaginary"

let dictCreationCanNotBeInlined = Js.Dict.fromArray(imaginaryExternalArgs)

open Js

let dictCreationCanBeInlined3 = Dict.fromArray([
  ("name", "hello"),
  ("age", "what"),
  ("more", "stuff"),
])

open Dict

let dictCreationCanBeInlined4 = fromArray([("name", "hello"), ("age", "what"), ("more", "stuff")])
