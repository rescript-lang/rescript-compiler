let x = {"age": 30}
let y = {"age": 30,}
let y = {"age": 30, "name": "steve"}
let y = {"age": 30, "name": "steve",}

// don't confuse with start of block expr
let x = {"age"}
let x = {"age"[0] } // doesn't make sense in practise
let x = {"age"->Js.log}
let x = {"age" ? true : false}
let x = {
  "age"->Js.log
  let foo = 1
  let bar = 2
  foo + bar
}
let x = {
  "age" ? true : false
  let foo = 1
  let bar = 2
  foo + bar
}
let x = {
  "age"[0] // doesn't make sense in practise
  let foo = 1
  let bar = 2
  foo + bar
}
