// Make sure labels matched don't "stick" on the builtin dict type
let stringDict = dict{
  "first": "hello",
}

let intDict = dict{
  "first": 1,
}

let foo = () => {
  let first = switch stringDict {
  | @res.dictPattern {first} => first ++ "2"
  | _ => "hello"
  }
  Js.log(first)
  let second = switch intDict {
  | @res.dictPattern {first} => first + 2
  | _ => 1
  }
  Js.log(second)
  let third = switch stringDict {
  | @res.dictPattern {first} => first ++ "2"
  | _ => "hello"
  }
  Js.log(third)
}
