module X: {
  type x = {
    name: string,
    age: int,
  }
  let make: int => string
} = {
  type x = {name: string, age: int}
  let make = s => {name: "", age: s}
}
