let f x y = Printf.printf "%d %d\n" x y

let i = ref 0
let () = f (incr i; !i) !i
