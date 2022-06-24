type myType = [
  | #first
  | #second
  | #third
  | #fourth
]

// just a quick way to make the switch compile
@val external x: myType = "myVariable"

switch x {
  | #first => Js.log("first")
}
