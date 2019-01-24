(* Simple example *)
let f x =
  (multimatch x with `A -> 1 | `B -> true),
  (multimatch x with `A -> 1. | `B -> "1");;

(* OK *)
module M : sig
  val f :
    [< `A & 'a = int & 'b = float | `B &   'b =string & 'a =  bool] -> 'a * 'b
end = struct let f = f end;;

(* Bad *)
module M : sig
  val f :
    [< `A & 'a = int & 'b = float | `B &   'b =string & 'a =   int] -> 'a * 'b
end = struct let f = f end;;

(* Should be good! *)
module M : sig
  val f :
    [< `A & 'a = int * float | `B & 'a = bool * string] -> 'a
end = struct let f = f end;;

let f = multifun `A|`B as x -> f x;;

(* Two-level example *)
let f = multifun
    `A -> (multifun `C -> 1 | `D -> 1.)
  | `B -> (multifun `C -> true | `D -> "1");;

(* OK *)
module M : sig
  val f :
    [< `A & 'b = [< `C & 'a = int | `D & 'a = float & 'c = bool] -> 'a
     | `B & 'b = [< `C & 'c = bool | `D & 'c = string] -> 'c] -> 'b
end = struct let f = f end;;

(* Bad *)
module M : sig
  val f :
    [< `A & 'b = [< `C & 'a = int | `D & 'a = bool] -> 'a
     | `B & 'b = [< `C & 'c = bool | `D & 'c = string] -> 'c] -> 'b
end = struct let f = f end;;

module M : sig
  val f :
    [< `A & 'b = [< `C & 'a = int | `D] -> 'a
     | `B & 'b = [< `C & 'c = bool | `D & 'c = string] -> 'c] -> 'b
end = struct let f = f end;;


(* Examples with hidden sharing *)
let r = ref []
let f = multifun `A -> 1 | `B -> true
let g x = r := [f x];;

(* Bad! *)
module M : sig
  val g : [< `A & 'a = int | `B & 'a = bool] -> unit
end = struct let g = g end;;

let r = ref []
let f = multifun `A -> r | `B -> ref [];;
(* Now OK *)
module M : sig
  val f : [< `A & 'b = int list ref | `B & 'b = 'c list ref] -> 'b
end = struct let f = f end;;
(* Still OK *)
let l : int list ref = r;;
module M : sig
  val f : [< `A & 'b = int list ref | `B & 'b = 'c list ref] -> 'b
end = struct let f = f end;;


(* Examples that would need unification *)
let f = multifun `A -> (1, []) | `B -> (true, [])
let g x = fst (f x);;
(* Didn't work, now Ok *)
module M : sig
  val g : [< `A & 'a * 'b = int * bool | `B & 'a * 'b = bool * int] -> 'a
end = struct let g = g end;;
let g = multifun (`A|`B) as x -> g x;;

(* Other examples *)

let f x =
  let a = multimatch x with `A -> 1 | `B -> "1" in
  (multifun `A -> print_int | `B -> print_string) x a
;;

let f = multifun (`A|`B) as x -> f x;;

type unit_op = [`Set of int | `Move of int]
type int_op = [`Get]

let op r =
  multifun
    `Get     -> !r
  | `Set x   -> r := x
  | `Move dx -> r := !r + dx
;;

let rec trace r = function
    [] -> []
  | op1 :: ops ->
      multimatch op1 with
        #int_op as op1 ->
          let x = op r op1 in
          x :: trace r ops
      | #unit_op as op1 ->
          op r op1;
          trace r ops
;;

class point x = object
  val mutable x : int = x
  method get = x
  method set y = x <- y
  method move dx = x <- x + dx
end;;

let poly sort coeffs x =
  let add, mul, zero =
    multimatch sort with
      `Int -> (+), ( * ), 0
    | `Float -> (+.), ( *. ), 0.
  in
  let rec compute = function
      []     -> zero
    | c :: cs -> add c (mul x (compute cs))
  in
  compute coeffs
;;

module M : sig
  val poly : [< `Int & 'a = int | `Float & 'a = float] -> 'a list -> 'a -> 'a
end = struct let poly = poly end;;

type ('a,'b) num_sort =
  'b constraint 'b = [< `Int & 'a = int | `Float & 'a = float]
module M : sig
  val poly : ('a,_) num_sort -> 'a list -> 'a -> 'a
end = struct let poly = poly end;;


(* type dispatch *)

type num = [ `Int | `Float ]
let print0 = multifun
    `Int -> print_int
  | `Float -> print_float
;;
let print1 = multifun
    #num as x -> print0 x
  | `List t -> List.iter (print0 t)
  | `Pair(t1,t2) -> (fun (x,y) -> print0 t1 x; print0 t2 y)
;;
print1 (`Pair(`Int,`Float)) (1,1.0);;
