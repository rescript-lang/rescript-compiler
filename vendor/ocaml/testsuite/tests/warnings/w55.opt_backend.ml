
let f = (fun x -> x + 1) [@inline never]

let g x = (f [@inlined]) x

let h = ref f

let i x = (!h [@inlined]) x

let j x y = x + y

let h x = (j [@inlined]) x

let a x =
  let b = x + 1 in
  fun y -> y + b

let b x y = (a [@inlined]) x y

let c x = x + 1 [@@inline never]
let d x = (c [@inlined]) x
