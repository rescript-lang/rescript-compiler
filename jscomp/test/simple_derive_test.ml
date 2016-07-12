
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


type nonrec t = int [@@bs.deriving{dynval}]

type nonrec tt = (int * string) list [@@bs.deriving{dynval}] 


let u = [%bs.deriving.dynval: t] 3 [@bs]


let h = [%bs.deriving.dynval: t list list array] [|[[3]]|] [@bs]

let hh = [%bs.deriving.dynval: tt list list array] [|[[[3,"3"]]]|] [@bs]
