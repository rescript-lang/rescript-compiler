let suites : Mt.pair_suites ref = ref []
let test_id = ref 0
let eq loc x y : unit = Mt.eq_suites ~test_id loc ~suites x y

type t =
  { x0: int
  ; x1: int
  ; x2: int
  ; x3: int
  ; x4: int
  ; x5: int
  ; x6: int
  ; x7: int
  ; x8: int
  ; x9: int
  ; x10: int
  ; x11: int
  ; x12: int
  ; x13: int
  ; x14: int
  ; x15: int
  ; x16: int
  ; x17: int
  ; x18: int
  ; x19: int
  ; x20: int
  ; x21: int
  ; x22: int }

let v0 : t =
  { x0= 9
  ; x1= 9
  ; x2= 9
  ; x3= 9
  ; x4= 9
  ; x5= 9
  ; x6= 9
  ; x7= 9
  ; x8= 9
  ; x9= 9
  ; x10= 9
  ; x11= 9
  ; x12= 9
  ; x13= 9
  ; x14= 9
  ; x15= 9
  ; x16= 9
  ; x17= 9
  ; x18= 9
  ; x19= 9
  ; x20= 9
  ; x21= 9
  ; x22= 9 }

let f0 (x : t) = {x with x0= 1}

type t1 =
  | A0 of
      { x0: int
      ; x1: int
      ; x2: int
      ; x3: int
      ; x4: int
      ; x5: int
      ; x6: int
      ; x7: int
      ; x8: int
      ; x9: int
      ; x10: int
      ; x11: int
      ; x12: int
      ; x13: int
      ; x14: int
      ; x15: int
      ; x16: int
      ; x17: int
      ; x18: int
      ; x19: int
      ; x20: int
      ; x21: int
      ; x22: int }
  | A1

let v1 : t1 =
  A0
    { x0= 9
    ; x1= 9
    ; x2= 9
    ; x3= 9
    ; x4= 9
    ; x5= 9
    ; x6= 9
    ; x7= 9
    ; x8= 9
    ; x9= 9
    ; x10= 9
    ; x11= 9
    ; x12= 9
    ; x13= 9
    ; x14= 9
    ; x15= 9
    ; x16= 9
    ; x17= 9
    ; x18= 9
    ; x19= 9
    ; x20= 9
    ; x21= 9
    ; x22= 9 }

let get_x0 (x : t1) = match x with A0 u -> Some u.x0 | _ -> None
let f1 (x : t1) : t1 = match x with A0 u -> A0 {u with x0= 1} | A1 -> A1

;;
eq __LOC__ (get_x0 (f1 v1)) (Some 1)

type t2 =
  | A0 of
      { x0: int
      ; x1: int
      ; x2: int
      ; x3: int
      ; x4: int
      ; x5: int
      ; x6: int
      ; x7: int
      ; x8: int
      ; x9: int
      ; x10: int
      ; x11: int
      ; x12: int
      ; x13: int
      ; x14: int
      ; x15: int
      ; x16: int
      ; x17: int
      ; x18: int
      ; x19: int
      ; x20: int
      ; x21: int
      ; x22: int }
  | A1 of int

let v2 : t2 =
  A0
    { x0= 9
    ; x1= 9
    ; x2= 9
    ; x3= 9
    ; x4= 9
    ; x5= 9
    ; x6= 9
    ; x7= 9
    ; x8= 9
    ; x9= 9
    ; x10= 9
    ; x11= 9
    ; x12= 9
    ; x13= 9
    ; x14= 9
    ; x15= 9
    ; x16= 9
    ; x17= 9
    ; x18= 9
    ; x19= 9
    ; x20= 9
    ; x21= 9
    ; x22= 9 }

let get_x0 (x : t2) = match x with A0 u -> Some u.x0 | _ -> None
let f2 (x : t2) : t2 = match x with A0 u -> A0 {u with x0= 1} | A1 _ -> x

;;
eq __LOC__ (get_x0 (f2 v2)) (Some 1)

type exn +=
  | A0 of
      { x0: int
      ; x1: int
      ; x2: int
      ; x3: int
      ; x4: int
      ; x5: int
      ; x6: int
      ; x7: int
      ; x8: int
      ; x9: int
      ; x10: int
      ; x11: int
      ; x12: int
      ; x13: int
      ; x14: int
      ; x15: int
      ; x16: int
      ; x17: int
      ; x18: int
      ; x19: int
      ; x20: int
      ; x21: int
      ; x22: int }

let f3 (x : exn) : exn = match x with A0 u -> A0 {u with x0= 1} | _ -> x
let get_x0 (x : exn) = match x with A0 u -> Some u.x0 | _ -> None

let v3 : exn =
  A0
    { x0= 9
    ; x1= 9
    ; x2= 9
    ; x3= 9
    ; x4= 9
    ; x5= 9
    ; x6= 9
    ; x7= 9
    ; x8= 9
    ; x9= 9
    ; x10= 9
    ; x11= 9
    ; x12= 9
    ; x13= 9
    ; x14= 9
    ; x15= 9
    ; x16= 9
    ; x17= 9
    ; x18= 9
    ; x19= 9
    ; x20= 9
    ; x21= 9
    ; x22= 9 }

;;
eq __LOC__ (get_x0 (f3 v3)) (Some 1)
;;
eq __LOC__ (get_x0 v3) (Some 9)
;;
eq __LOC__ (get_x0 Not_found) None
;;
Mt.from_pair_suites __MODULE__ !suites
