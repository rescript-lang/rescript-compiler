let rec x = let y = () in x;;

let rec x = let module M = struct let f = x let g = x () end in fun () -> ();;

let rec x = let module M = struct let f = x () let g = x end in fun () -> ();;

let rec x = (let module M = struct let f = y 0 let g = () end in fun () -> ())
    and y = succ;;

let rec x = let module M = struct module N = struct let y = x end end in M.N.y;;

let rec x = let module M = struct let f = x () and g = x end in fun () -> ();;

class c _ = object end;;
let rec x = new c x;;

let rec x = ignore x;;

let rec x = y 0 and y _ = ();;

let rec c = { c with Complex.re = 1.0 };;

let rec b = if b then true else false;;

let r = ref ()
let rec x = r := x;;

let rec x =
  for i = 0 to 1 do
    let z = y in ignore z
  done
and y = x; ();;

let rec x =
  for i = 0 to y do
    ()
  done
and y = 10;;

let rec x =
  for i = y to 10 do
    ()
  done
and y = 0;;

let rec x =
  while false do
    let y = x in ignore y
  done
and y = x; ();;

let rec x =
  while y do
    ()
  done
and y = false;;

let rec x =
  while y do
    let y = x in ignore y
  done
and y = false;;

let rec x = y#m and y = object method m = () end;;

let rec x = (object method m _ = () end)#m x;;

let rec x = y.contents and y = { contents = 3 };;

let rec x = object val mutable v = 0 method m = v <- y end and y = 1;;

let rec x = assert y and y = true;;

let rec x = object method m = x end;;

let rec x = object method m = ignore x end;;

(* The builtin Pervasives.ref is currently treated as a constructor.
   Other functions of the same name should not be so treated. *)
let _ =
  let module Pervasives =
  struct
    let ref _ = assert false
  end in
  let rec x = Pervasives.ref y
  and y = fun () -> ignore x
  in (x, y)
;;

(* An example, from Leo White, of let rec bindings that allocate
   values of unknown size *)
let foo p x =
  let rec f =
    if p then (fun y -> x + g y) else (fun y -> g y)
  and g =
    if not p then (fun y -> x - f y) else (fun y -> f y)
  in
  (f, g)
;;

module type T = sig end
let rec x = (module (val y : T) : T)
and y = let module M = struct let x = x end in (module M : T)
;;

let rec x =
  match let _ = y in raise Not_found with
    _ -> "x"
  | exception Not_found -> "z" 
and y = match x with
  z -> ("y", z);;

