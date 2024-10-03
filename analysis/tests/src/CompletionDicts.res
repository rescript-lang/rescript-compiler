// let dict = Js.Dict.fromArray([])
//                               ^com

// let dict = Js.Dict.fromArray([()])
//                                ^com

// let dict = Js.Dict.fromArray([("key", )])
//                                      ^com

// ^in+
let dict = Js.Dict.fromArray([
  ("key", true),
  //  ("key2", )
  //          ^com
])
// ^in-
