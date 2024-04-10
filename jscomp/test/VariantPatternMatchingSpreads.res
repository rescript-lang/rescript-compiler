type a = One | Two | Three
type a1 = One
type b = | ...a | Four | Five

let doWithA = (a: a) => {
  switch a {
  | One => Js.log("aaa")
  | Two => Js.log("twwwoooo")
  | Three => Js.log("threeeee")
  }
}

let doWithB = (b: b) => {
  switch b {
  | One => Js.log("aaa")
  | _ => Js.log("twwwoooo")
  }
}

let lookup = (b: b) =>
  switch b {
  | ...a as a => doWithA(a)
  | Four => Js.log("four")
  | Five => Js.log("five")
  }

let lookup2 = (b: b) =>
  switch b {
  | ...a => Js.log("spread")
  | Four => Js.log("four")
  | Five => Js.log("five")
  }
