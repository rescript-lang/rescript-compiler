module X: {
  type x = {
    name: string,
    age: int,
  }
  let make: (~name: string, ~age: int) => x
} = {
  type x = {name: string, age: int}
  let make = (~name, ~age) => {name, age: age->int_of_float}
}
