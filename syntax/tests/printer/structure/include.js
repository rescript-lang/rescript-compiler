include Belt

@attr
include WebGl

include (
  /* Use varargs to avoid the ReactJS warning for duplicate keys in children */
  {
    @val @module("react")
    external createElementInternalHack: 'a = "createElement"
    @bs.send
    external apply: (
      'theFunction,
      'theContext,
      'arguments,
    ) => 'returnTypeOfTheFunction = "apply"

    let createElementVariadic = (domClassName, ~props=?, children) => {
      let variadicArguments =
        [Obj.magic(domClassName), Obj.magic(props)] |> Js.Array.concat(children)
      createElementInternalHack->apply(Js.Nullable.null, variadicArguments)
    }
  }: {
    let createElementVariadic: (
      string,
      ~props: props=?,
      array<React.element>,
    ) => React.element
  }
)
