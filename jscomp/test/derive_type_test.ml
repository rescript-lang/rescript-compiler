#if 0 then
type t = 
  | Monday
  | Tuesday
  | SpecialDay of int 
  | A of int * int 
  | B of (int * int) 

  | D of t 
        [@@bs.deriving {dynval } ]

type h = 
  { x : int ; y : float ; z : string }
    [@@bs.deriving {dynval}]

(*
let all_fields_of_h : Bs_dyn.record_shape = 
  { labels = [| "x" ; "y"; "z"|]}

let value_of_h : h Bs_dyn.to_value = 
  fun [@bs] (value : h)
    -> match value with 
    | {x ; y ; z} -> 
        Bs_dyn.value_of_record all_fields_of_h
          [| Bs_dyn.value_of_int x [@bs]; 
             Bs_dyn.value_of_float y [@bs];
             Bs_dyn.value_of_string z [@bs]
          |]
*)

type u = int 
    (* [@@bs.deriving {dynval ; dyntype } ] *)
and v = u 
    (* [@@bs.deriving {dynval ; dyntype } ] *)

let x = 3 

type nonrec x = 
  | A of int 
  | B of int 


type nonrec vv = { x : int ; y : string }
    [@@bs.deriving {dynval}]
#end