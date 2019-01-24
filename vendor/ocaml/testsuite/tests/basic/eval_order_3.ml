let i = ref 0

let f x y =
  Printf.printf "%d %d\n" x y;
  0
[@@inline never]

let foo _ = ()

let foobar baz =
  let incr_i _ =
    incr i;
    !i
  in
  let b = !i in
  let z = foo 42 in
  let a = (incr_i [@inlined never]) z in
  let x = f a b in
  x + 1

let () =
  ignore ((foobar 0) : int)
