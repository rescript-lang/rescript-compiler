


external eqNull : 'a -> box:'a Js.null -> bool = "%bs_equal_null"
external eqUndefined : 'a -> box:'a Js.undefined -> bool = "%bs_equal_undefined"

external eqNullable : 'a -> box:'a Js.nullable -> bool = "%bs_equal_nullable" 


let aa,bb,cc = eqNull, eqUndefined,  eqNullable



let suites = ref []
let test_id =ref 0

let eq loc x y = Mt.eq_suites ~suites ~test_id loc x y 
let b loc x  = Mt.bool_suites ~suites ~test_id loc x 

let f () = None 
let shouldBeNull () = Js.null 

let () = 
  b __LOC__ (not (eqNull 3 ~box:Js.null));
  b __LOC__ (not (eqNull None ~box:Js.null));
  b __LOC__ (not (eqNull "3" Js.null));
  b __LOC__ (not (eqNull '3' Js.null));
  b __LOC__ (not (eqNull 0L Js.null));
  b __LOC__ (not (eqNull 0n Js.null));
  b __LOC__ (not (eqNull 0. Js.null));
  b __LOC__ (not (eqNull (f ()) Js.null));
  b __LOC__ ( (eqNull (shouldBeNull ()) Js.null));
  b __LOC__ (not (eqNull 1 (Js.Null.return 3)));
  b __LOC__ ( (eqNull None (Js.Null.return None)));
  b __LOC__ (not (eqNull (Some 3) (Js.Null.return None)))

let () = 
  let v = Js.Nullable.null in 
  b __LOC__ (not (eqNullable 3 ~box:v));
  b __LOC__ (not (eqNullable None ~box:v));
  b __LOC__ (not (eqNullable "3" v));
  b __LOC__ (not (eqNullable '3' v));
  b __LOC__ (not (eqNullable 0L v));
  b __LOC__ (not (eqNullable 0n v));
  b __LOC__ (not (eqNullable 0. v));
  b __LOC__ (not (eqNullable (f ()) v));
  b __LOC__ ( (eqNullable (shouldBeNull ()) v));
  b __LOC__ (not (eqNullable 1 (Js.Nullable.return 3)));
  b __LOC__ ( (eqNullable None (Js.Nullable.return None)));
  b __LOC__ (not (eqNullable (Some 3) (Js.Nullable.return None)))

let () = 
  let v = Js.Undefined.empty in 
  
  b __LOC__ (not (eqUndefined 3 ~box:v));
  b __LOC__ (not (eqUndefined None ~box:v));
  b __LOC__ (not (eqUndefined "3" v));
  b __LOC__ (not (eqUndefined '3' v));
  b __LOC__ (not (eqUndefined 0L v));
  b __LOC__ (not (eqUndefined 0n v));
  b __LOC__ (not (eqUndefined 0. v));
  b __LOC__ (not (eqUndefined (f ()) v));
  b __LOC__ (not (eqUndefined (shouldBeNull ()) v));
  b __LOC__ (not (eqUndefined 1 (Js.Undefined.return 3)));
  b __LOC__ ( (eqUndefined None (Js.Undefined.return None)));
  b __LOC__ (not (eqUndefined (Some 3) (Js.Undefined.return None)))


  

;; Mt.from_pair_suites __LOC__ !suites 
