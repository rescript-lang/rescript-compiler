let name = "Steve"
let x = 42

let numbersArray = [1, 2, 3, 4, 5]
let numbersTuple = (1, 2, 3, 4, 5)
let numbersList = list{1, 2, 3, 4, 5}

let add = (a, b) => a + b

type s = string

type user = {
  name: string,
  age: int,
}

type user2 = {
  naaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaame: string,
  aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaage: int,
  emaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaail: string
}

module Diff = {
  let string = (s1: string, s2: string) => s1 != s2
}

module Diff2 = Diff

type rec tree<'value> =
  | Nil
  | Node(tree<'value>, 'value, tree<'value>) 

type intTree = tree<int>

type rec tree2<'value> =
  | Niiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiil2
  | Noooooooooooooooooooooooooooooooooooode2(tree2<'value>, 'value, tree2<'value>) 

type rec tree3<'value> =
  | Nil3
  | Node3({left: tree3<'value>, value: 'value, right: tree3<'value>}) 

type rec tree4<'value> =
  | Nil4
  | Node4({leeeeeeeeeeeeeeeeeeeeeeeeeeeeft: tree3<'value>, vaaaaaaaaaaaaaaaaaaaaalue: 'value, riiiiiiiiiiiiiiiiiiiiiiight: tree3<'value>}) 

type color = ..

type color += Blue
type color += | Red | Green

type color += | Blaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaack | Oraaaaaaaaaaaaaaaaaaaaaaaaaaaaaaange | Reeeeeeeeeeeeeeeeeeeed

module Expr = {
  type attr = ..

  type attr += private Str(string)

  type attr +=
    | Int(int)
    | Float(float)
}

module User = {
  type t = {name: string, age: int}
}

type userT = User.t = {name: string, age: int}
