let withOpt = (~x=1, y) => (~z=1, w) => x+y+z+w
let testWithOpt = withOpt(3)(4)
let partial = withOpt(~x=10, 3)(~z=4, 11)
let total = withOpt(. ~x=10, 3)(. ~z=4, 11)

module M: {
  let foo: (unit => int) => int
} = {
  let foo = func => func() + 1
}
