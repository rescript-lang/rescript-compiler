let name = "Steve"
let x = 42
let pi = 3.14

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

type veeeeeeeeeeeeeeeeeeeeeeeeryLongTypeAlias = string

type rgb = [#Red | #Green | #Blue]
type longRgb = [#Reeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeed | #Greeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeen | #Blueeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee | #Rrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrgb(float, float, float) | #Rrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrgb2(veeeeeeeeeeeeeeeeeeeeeeeeryLongTypeAlias, veeeeeeeeeeeeeeeeeeeeeeeeryLongTypeAlias, veeeeeeeeeeeeeeeeeeeeeeeeryLongTypeAlias)]
let red = #Red
let red2: rgb = #Red

let int = #Int(123)
let int2 = #Int("abc", true)
let int3 = #Int(1.0, 2.0, 3.0)

type customRgb = [#Rbg(float, float, float)]
let color = #Rbg(1.0, 0.6, 0.2)
let tupleAsSingleArg = #tuple((1.3, 4.2))
let oneArg = #One("oneArgument")

type redColor = [ #Red ]
type greenColor = [ #Green ]
type blueColor = [ #Blue ]
type rgbColor = [ redColor | greenColor | blueColor ]

module M = {
  type data = [#IntData(int) | #StrData(string)]
  let stringOfData = (x: data) =>
    switch x {
    | #IntData(i) => string_of_int(i)
    | #StrData(s) => s
    }
}
let y = M.stringOfData(#IntData(123))

type rgbw = [#Red | #Green | #Blue | #White ]

let id = (x: [> #Red | #Green | #Blue]) => x
let id = (x: [> rgb]) => x

type point = [ | #Point(float, float) ]
type shape = [ | #Rectangle(point, point) | #Circle(point, float) ]

let pi = 4.0 *. atan(1.0)
let computeArea = (s: shape) =>
  switch s {
  | #Rectangle(#Point(x1, y1), #Point(x2, y2)) =>
    let width = abs_float(x2 -. x1)
    let height = abs_float(y2 -. y1)
    width *. height
  | #Circle(_, radius) => pi *. radius ** 2.0
  }

let shoelaceFormula = (#Point(x1, y1), #Point(x2, y2), #Point(x3, y3)) =>
  0.5 *.
  abs_float(
    x1 *. y2 -. x3 *. y2 +. x3 *. y1 -. x1 *. y3 +. x2 *. y3 -. x2 *. y1,
  )

type shapePlus = [
| #Rectangle(point, point)
| #Circle(point, float)
| #Triangle(point, point, point)
]

let computeAreaPlus = (sp: shapePlus) =>
  switch sp {
  | #Triangle(p1, p2, p3) => shoelaceFormula(p1, p2, p3)
  | #...shape as s => computeArea(s)
  }

let top = #Point(3.0, 5.0)
let left = #Point(0.0, 0.0)
let right = #Point(3.0, 0.0)

let circ = #Circle(top, 3.0)
let tri = #Triangle(top, left, right)

let x = computeAreaPlus(circ)
let y = computeAreaPlus(tri)

let basic_color_to_int = color =>
  switch color {
  | #Black => 0
  | #Red => 1
  | #Green => 2
  | #Yellow => 3
  | #Blue => 4
  | #Magenta => 5
  | #Cyan => 6
  | #White => 7
  }

let color_to_int = color =>
  switch color {
  | #Basic(basic_color, weight) =>
    let base = switch weight {
    | #Bold => 8
    | #Regular => 0
    }
    base + basic_color_to_int(basic_color)
  | #RGB(r, g, b) => 16 + b + g * 6 + r * 36
  | #Gray(i) => 232 + i
  }

module type Conjunctive = {
  type u1 = [ | #A | #B]
  type u2 = [ | #A | #B | #C]

  let f: [< | #T([< u2]) & ([< u2]) & ([< u1])] => unit
  let g: [< | #S&([< u2]) & ([< u2]) & ([< u1])] => unit
};

// exotic idents in poly-vars
type t20 = [#\"type"]
type t21 = [#\"va r ia nt"]
type t22 = [#\"Variant ⛰"]
type \"let" = int
type \"type" = [ #\"Point🗿"(\"let", float) ]

type exoticUser = {
  \"let": string,
  \"type": float
}
