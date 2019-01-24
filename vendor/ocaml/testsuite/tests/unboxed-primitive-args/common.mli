(** Type of arguments/result *)
type 'a typ =
  | Int       : int       typ
  | Int32     : int32     typ
  | Int64     : int64     typ
  | Nativeint : nativeint typ
  | Float     : float     typ

type 'a proto =
  | Ret : 'a typ -> 'a proto
  | Abs : 'a typ * 'b proto -> ('a -> 'b) proto

(** Same as [Abs]. We choose this operator for its associativity. *)
val ( ** ) : 'a typ -> 'b proto -> ('a -> 'b) proto

type test =
  | T1 : string * ('a -> 'b) * 'a typ * 'b typ -> test
  | T2 : string * ('a -> 'b -> 'c) * 'a typ * 'b typ * 'c typ -> test
  | T3 : string * ('a -> 'b -> 'c -> 'd) *
         'a typ * 'b typ * 'c typ * 'd typ -> test
  | T4 : string * ('a -> 'b -> 'c -> 'd -> 'e) *
         'a typ * 'b typ * 'c typ * 'd typ * 'e typ -> test
  | T5 : string * ('a -> 'b -> 'c -> 'd -> 'e -> 'f) *
         'a typ * 'b typ * 'c typ * 'd typ * 'e typ * 'f typ -> test
  | T6 : string * ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g) *
         'a typ * 'b typ * 'c typ * 'd typ * 'e typ * 'f typ * 'g typ -> test
  | T : string * 'a * 'a proto -> test

val run_tests : test list -> unit
