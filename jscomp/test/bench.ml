let map f a =
  let l = Array.length a in
  if l = 0 then [||]
  else
    let r = Array.make l (f (Array.unsafe_get a 0) [@bs]) in
    for i = 1 to l - 1 do
      Array.unsafe_set r i (f (Array.unsafe_get a i) [@bs])
    done ;
    r

let map (type u v) (f : u -> v) (a : u array) : v array =
  map (fun [@bs] x -> f x) a

let init l f =
  if l = 0 then [||]
  else if l < 0 then invalid_arg "Array.init"
    (* See #6575. We could also check for maximum array size, but this depends
       on whether we create a float array or a regular one... *)
  else
    let res = Array.make l (f 0 [@bs]) in
    for i = 1 to pred l do
      Array.unsafe_set res i (f i [@bs])
    done ;
    res

let init l f = init l (fun [@bs] x -> f x)

let fold_left f x a =
  let r = ref x in
  for i = 0 to Array.length a - 1 do
    r := (f !r (Array.unsafe_get a i) [@bs])
  done ;
  !r

let fold_left f x a = fold_left (fun [@bs] x y -> f x y) x a

let f2 () =
  let arr = init 3_000_000 (fun i -> float_of_int i) in
  let b = map (fun i -> i +. i -. 1.) arr in
  let v = fold_left ( +. ) 0. b in
  print_endline (string_of_float v)

;;
f2 ()

(* local variables: *)
(* compile-command: "ocamlc bench.ml -o bench.byte && js_of_ocaml --opt 3
   bench.byte -o bench.1.js " *)
(* end: *)
