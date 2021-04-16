@@config({
  flags: [
  "-w","-16"  
    ],
})
module Test: {
  @obj
  external makeProps: (~s: string=?, unit) => {"s": string} = ""
} = {
  @obj
  external makeProps: (~s: 's=?, unit) => {"s": 's} = ""
}

let u = Test.makeProps(~s="hello", ())

let f = (~s=?, y) => {
  Test.makeProps(~s?, ())->Js.log
  Js.log(y)
}

module H: {
  @react.component
  let make: (~s: string=?) => React.element
} = {
  @react.component
  let make = (~s=?) =>
    switch s {
    | Some(s) => React.string(s)
    | None => React.null
    }
}
