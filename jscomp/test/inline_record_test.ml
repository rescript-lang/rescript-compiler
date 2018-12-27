

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

let v2 : t1 = A0 {lbl = 3; more = []}

type t2 = ..

type t2 += | A0 of {lbl: int ; more : int list}

let v3 : t2 = A0 {lbl = 3; more = []}    


type t3 = 
  | A0 of {lbl : int; more : int list}
  | A1

let vvv : t3 = A0 {lbl = 3; more = []}  


;; eq __LOC__ (match v3 with A0 {lbl} -> lbl | _ -> assert false) 3 

type t4 = 
  | A0 of { mutable x : int; y :int ; mutable z : int }
  | A1 of { mutable z : int }

let ff (x : t4) =   
  match x with 
  | A0 u -> u.x <- u.x + 1 
  | A1 u -> u.z <- u.z + 2

let v4 : t4 =  A0 { x = 0; y = 0; z = 0}
let v5 : t4 =  A1 { z = 0 }

;; for i = 0 to 10 do 
    ff v4; ff v5
done 

;; eq __LOC__ (match v4 with A0 u -> u.x | _ -> assert false) 11

;; eq __LOC__ (match v5 with A1 u -> u.z | _ -> assert false) 22


exception A4 of {mutable x : int ; y : int ; mutable z : int}

let v6 : exn = A4 { x = 0; y = 0; z = 0}

let ff0 (x : exn) = 
  match x with 
  | A4 u -> u.x <- u.x + 1 ; u.z <- u.z + 1   
  | _ -> ()

;; for i = 0 to 10 do   
  ff0 v6
done 

;; eq __LOC__ (match v6 with A4 u -> u.x  | _ -> assert false) 11 
;; Mt.from_pair_suites __MODULE__ !suites

let ff1 (x : t1) : t1 =  
  match x with 
  | A0 u -> A0 {u with lbl = u.lbl + 1}
  | A1 -> A1 

#end