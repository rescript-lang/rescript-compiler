let foo ~bar = ignore bar (* one label *)

let bar ~foo ~baz = ignore (foo, baz) (* two labels *)

let () = foo 2
let () = bar 4 2
