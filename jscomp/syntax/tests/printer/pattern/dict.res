let someDict = dict{
    "one": "one",
}

let dict{"one": ?one} = someDict

let foo = () => switch someDict {
| dict{"one": "one"} => Js.log("one")
| _ => Js.log("not one")
}