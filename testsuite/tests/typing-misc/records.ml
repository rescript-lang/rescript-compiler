(* undefined labels *)
type t = {x:int;y:int};;
{x=3;z=2};;
fun {x=3;z=2} -> ();;

(* mixed labels *)
{x=3; contents=2};;

(* private types *)
type u = private {mutable u:int};;
{u=3};;
fun x -> x.u <- 3;;

(* Punning and abbreviations *)
module M = struct
  type t = {x: int; y: int}
end;;

let f {M.x; y} = x+y;;
let r = {M.x=1; y=2};;
let z = f r;;

(* messages *)
type foo = { mutable y:int };;
let f (r: int) = r.y <- 3;;

(* bugs *)
type foo = { y: int; z: int };;
type bar = { x: int };;
let f (r: bar) = ({ r with z = 3 } : foo)

type foo = { x: int };;
let r : foo = { ZZZ.x = 2 };;

(ZZZ.X : int option);;

(* PR#5865 *)
let f (x : Complex.t) = x.Complex.z;;
