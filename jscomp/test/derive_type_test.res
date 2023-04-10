/*
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
*/

/* [@@bs.deriving {dynval ; dyntype } ] */

/* [@@bs.deriving {dynval ; dyntype } ] */
