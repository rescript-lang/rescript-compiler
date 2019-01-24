
let f x = x + 1
let g x = x - 1

let run () =
  let r = ref f in
  r := g;
  let n = !r 1 in
  assert(n = 0)
