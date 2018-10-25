

#if OCAML_VERSION =~ "<4.03.0" then
#else

let suites : Mt.pair_suites ref =  ref []

let test_id = ref 0

let eq loc x y : unit = Mt.eq_suites ~test_id loc  ~suites x y

type t0 = 
    | A0  of {lbl : int ; more : int list}
    | A1  of {more : int list}

let v : t0 = A0 {lbl = 3; more = []}
let v1 = A1 {more = [1;2]}

let f (x : t0) = 
  match x with 
  | A0 {lbl; more}
    -> List.fold_left (+)  lbl more 
  | A1 {more} ->  
      List.fold_left (+) 0 more  
;; eq __LOC__ (f v) 3
;; eq __LOC__ (f v1) 3 
;; Js.log (f v)      
;; Js.log (f v1)

(* let foo ?(bar= 1) baz = bar + baz *)
type t1 = 
    | A0  of {lbl : int ; more : int list}
    | A1  

let v : t1 = A0 {lbl = 3; more = []}

type t2 = ..

type t2 += | A0 of {lbl: int ; more : int list}

let v : t2 = A0 {lbl = 3; more = []}    


type t3 = 
  | A0 of {lbl : int; more : int list}
  | A1

let vvv : t3 = A0 {lbl = 3; more = []}  

;; Mt.from_pair_suites __FILE__ !suites

#end