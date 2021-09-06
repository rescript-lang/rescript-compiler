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

let computeAreaExotic = (sp) =>
  switch sp {
  | #"R-Triangle+"(_p1, _p2, _p3) => ()
  | #...shape as s => ignore(s); ()
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
type t20 = [#"type"]
type t21 = [#"va r ia nt"]
type t22 = [#"Variant ⛰"]
type \"let" = int
type \"type" = [ #"Point🗿"(\"let", float) ]
type t23 = [
  | #"1"
  | #"10space"
  | #"123"
]

type exoticUser = {
  \"let": string,
  \"type": float
}

module Js = {
  type t<'a> = 'a

  module Fn = {
    type arity0<'a> = {i0: unit => 'a}
    type arity1<'a> = {i1: 'a}
    type arity2<'a> = {i2: 'a}
    type arity3<'a> = {i3: 'a}
    type arity4<'a> = {i4: 'a}
    type arity5<'a> = {i5: 'a}
    type arity6<'a> = {i6: 'a}
    type arity7<'a> = {i7: 'a}
    type arity8<'a> = {i8: 'a}
    type arity9<'a> = {i9: 'a}
    type arity10<'a> = {i10: 'a}
    type arity11<'a> = {i11: 'a}
    type arity12<'a> = {i12: 'a}
    type arity13<'a> = {i13: 'a}
    type arity14<'a> = {i14: 'a}
    type arity15<'a> = {i15: 'a}
    type arity16<'a> = {i16: 'a}
    type arity17<'a> = {i17: 'a}
    type arity18<'a> = {i18: 'a}
    type arity19<'a> = {i19: 'a}
    type arity20<'a> = {i20: 'a}
    type arity21<'a> = {i21: 'a}
    type arity22<'a> = {i22: 'a}
  }
}

type arity0 = Js.Fn.arity0<unit>
type arity0b = Js.Fn.arity0<int>
type arity1 = Js.Fn.arity1<(int) => int>
type arity2 = Js.Fn.arity2<(int, int) => int>
type arity3 = Js.Fn.arity3<(int, int, int) => int>
type arity4 = Js.Fn.arity4<(int, int, int, int) => int>
type arity5 = Js.Fn.arity5<(int, int, int, int, int) => int>
type arity6 = Js.Fn.arity6<(int, int, int, int, int, int) => int>
type arity7 = Js.Fn.arity7<(int, int, int, int, int, int, int) => int>
type arity8 = Js.Fn.arity8<(int, int, int, int, int, int, int, int) => int>
type arity9 = Js.Fn.arity9<(int, int, int, int, int, int, int, int, int) => int>
type arity10 = Js.Fn.arity10<(int, int, int, int, int, int, int, int, int, int) => int>
type arity11 = Js.Fn.arity11<(int, int, int, int, int, int, int, int, int, int, int) => int>
type arity12 = Js.Fn.arity12<(int, int, int, int, int, int, int, int, int, int, int, int) => int>
type arity13 = Js.Fn.arity13<(int, int, int, int, int, int, int, int, int, int, int, int, int) => int>
type arity14 = Js.Fn.arity14<(int, int, int, int, int, int, int, int, int, int, int, int, int, int) => int>
type arity15 = Js.Fn.arity15<(int, int, int, int, int, int, int, int, int, int, int, int, int, int, int) => int>
type arity16 = Js.Fn.arity16<(int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int) => int>
type arity17 = Js.Fn.arity17<(int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int) => int>
type arity18 = Js.Fn.arity18<(int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int) => int>
type arity19 = Js.Fn.arity19<(int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int) => int>
type arity20 = Js.Fn.arity20<(int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int) => int>
type arity21 = Js.Fn.arity21<(int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int) => int>
type arity22 = Js.Fn.arity22<(int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int) => int>

type obj1<'a> = option<Js.t<{"a": int}>>
type obj2 = Js.t<{"a": int}>
type obj3 = Js.t<{. "a": int}>
type obj4 = {"a": int}
type obj5<'a> = Js.t<{..} as 'a>
type obj6 = {"a": int}
type obj7 = {. "a": int}
type obj8<'a> = {.. "a": int} as 'a

type objUser = {"name": string, "age": int}
type objUserWithLongFields = {"name": string, "x": int, "age": int, "looooooongFiiiiiiiieeeeeeeldName": string, "anoooooooooooooooootherLongFiiiiiieeeeeeeldName": int}

type objectCoordinate =  {"x": float, "y": float}
type threeDimensionalCoordinate = {...objectCoordinate, "z": float}
type differentSpreadedCoordinate = {"z": float, ...objectCoordinate, "alpha": int}

type multiSpreadedCoordinate = {
  ...threeDimensionalCoordinate,
  "a": int,
  ...differentSpreadedCoordinate,
  "b": int
}

type dotdotObjectCoordinate<'a> =  {.. "suuuuuuuuuuuperLooooooooooooongFieldNaaaaaaaaaaaame": float, "suuuuuuuuuuuperLooooooooooooongFieldNaaaaaaaaaaaame2222222222222": float} as 'a

type permissions = [
   | #777
   | #644
 ]

 type numericPolyVarWithPayload = [
   | #1(string)
   | #2(int, string)
 ]

 let numericPolyVarMatch = switch #644 {
   | #777 => #1("payload")
   | #644 => #2(42, "test")
 }

let sort = (type s, module(Set: Set.S with type elt = s), l) =>
  Set.elements(List.fold_right(Set.add, l, Set.empty))

let make_set = (type s, cmp) => {
  module S = Set.Make({
    type t = s
    let compare = cmp
  })
  module(S: Set.S with type elt = s)
}

type picture = string

module type DEVICE = {
  let draw: picture => unit
}

let devices: Hashtbl.t<string, module(DEVICE)> = Hashtbl.create(17)

module rec A: {
  type t =
    | Leaf(string)
    | Node(ASet.t)
  let compare: (t, t) => int
} = {
  type t =
    | Leaf(string)
    | Node(ASet.t)
  let compare = (t1, t2) =>
    switch (t1, t2) {
    | (Leaf(s1), Leaf(s2)) => compare(s1, s2)
    | (Leaf(_), Node(_)) => 1
    | (Node(_), Leaf(_)) => -1
    | (Node(n1), Node(n2)) => ASet.compare(n1, n2)
    }
}
and ASet: Set.S with type elt = A.t = Set.Make(A)

type emptyObject = {.}

let f = (~x=?, ~y as _) => x

type call = CleanStart

let f = (~a=1) => 1
