#if 0 then


type nonrec vv = 
  { x : int ;
    y : string ; 
    a : int ;
    b : float ;
    c : int32 ;
    tuple : int * string * string list * 
            float list * string array array * int 
  }
  [@@bs.deriving {dynval}]

type nonrec uu = 
  | A of int 
  | B of int * string 
  | C of vv * int option option
  | D of int array array
  | E of (int * int )
[@@bs.deriving {dynval}]

type t = int [@@bs.deriving{dynval}]

val u : Bs_dyn.value
val h : Bs_dyn.value
val hh : Bs_dyn.value 

(**
{[ 
let f u = [%bs.deriving.dynval: uu] u [@bs]
let f v = [%bs.deriving.dynval: vv] v [@bs]

[%bs.deriving.dynval int list]
]}
*)
#end