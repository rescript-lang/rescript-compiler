type 'a ty = 
  | Int : int ty
  | Bool : bool ty

let fbool (type t) (x : t) (tag : t ty) = 
  match tag with 
  | Bool -> x
;;
(* val fbool : 'a -> 'a ty -> 'a = <fun> *)
(** OK: the return value is x of type t **)

let fint (type t) (x : t) (tag : t ty) = 
  match tag with 
  | Int -> x > 0
;;
(* val fint : 'a -> 'a ty -> bool = <fun> *)
(** OK: the return value is x > 0 of type bool;
This has used the equation t = bool, not visible in the return type **)

let f (type t) (x : t) (tag : t ty) = 
  match tag with 
  | Int -> x > 0
  | Bool -> x
(* val f : 'a -> 'a ty -> bool = <fun> *)


let g (type t) (x : t) (tag : t ty) = 
  match tag with 
  | Bool -> x
  | Int -> x > 0
(* Error: This expression has type bool but an expression was expected of type
t = int *)

let id x = x;;
let idb1 = (fun id -> let _ = id true in id) id;;
let idb2 : bool -> bool = id;;
let idb3 ( _ : bool ) = false;;

let g (type t) (x : t) (tag : t ty) =
  match tag with
  | Bool -> idb3 x
  | Int -> x > 0

let g (type t) (x : t) (tag : t ty) =
  match tag with
  | Bool -> idb2 x
  | Int -> x > 0

