type ('a0, 'a1) t = ('a0 -> 'a1 [@bs])

let f0 = fun [@bs] () -> 0
let f1 = fun [@bs] a0 -> a0
let f2 = fun [@bs] a0 a1 -> (a0,a1)
let f3= fun [@bs]a0 a1 a2 -> ( a0,a1,a2)
let f4= fun [@bs]a0 a1 a2 a3 -> ( a0,a1,a2,a3)
let f5= fun [@bs]a0 a1 a2 a3 a4 -> ( a0,a1,a2,a3,a4)
let f6= fun [@bs]a0 a1 a2 a3 a4 a5 -> ( a0,a1,a2,a3,a4,a5)
let f7= fun [@bs]a0 a1 a2 a3 a4 a5 a6 -> ( a0,a1,a2,a3,a4,a5,a6)
let f8= fun [@bs]a0 a1 a2 a3 a4 a5 a6 a7 -> ( a0,a1,a2,a3,a4,a5,a6,a7)
let f9= fun [@bs]a0 a1 a2 a3 a4 a5 a6 a7 a8 -> ( a0,a1,a2,a3,a4,a5,a6,a7,a8)
let f10= fun [@bs]a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 -> ( a0,a1,a2,a3,a4,a5,a6,a7,a8,a9)
let f11= fun [@bs]a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 -> ( a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10)
let f12= fun [@bs]a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 -> ( a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11)
let f13= fun [@bs]a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 -> ( a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12)
let f14= fun [@bs]a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 -> ( a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13)
let f15= fun [@bs]a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 -> ( a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14)
let f16= fun [@bs]a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 -> ( a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15)
let f17= fun [@bs]a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 -> ( a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16)
let f18= fun [@bs]a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 -> ( a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17)
let f19= fun [@bs]a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 -> ( a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18)
let f20= fun [@bs]a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 -> ( a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19)
let f21= fun [@bs]a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 -> ( a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20)
let f22= fun [@bs]a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 -> ( a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21)
(* let f23= fun [@bs]a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 -> ( a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21) *)
(* TODO: better errror message than Unbound record field Js.Fn.I_23 *)
;; f0 () [@bs] |. Js.log 
;; f1 0 [@bs] |. Js.log

;; f2 0 1 [@bs] |. Js.log 
;; f3 0 1 2 [@bs] |. Js.log 
;; f4 0 1 2 3 [@bs] |. Js.log 
;; f5 0 1 2 3 4 [@bs] |. Js.log 
;; f6 0 1 2 3 4 5 [@bs] |. Js.log 
;; f7 0 1 2 3 4 5 6 [@bs] |. Js.log 
;; f8 0 1 2 3 4 5 6 7 [@bs] |. Js.log 
;; f9 0 1 2 3 4 5 6 7 8 [@bs] |. Js.log 
;; f10 0 1 2 3 4 5 6 7 8 9 [@bs] |. Js.log 
;; f11 0 1 2 3 4 5 6 7 8 9 10 [@bs] |. Js.log 
;; f12 0 1 2 3 4 5 6 7 8 9 10 11 [@bs] |. Js.log 
;; f13 0 1 2 3 4 5 6 7 8 9 10 11 12 [@bs] |. Js.log 
;; f14 0 1 2 3 4 5 6 7 8 9 10 11 12 13 [@bs] |. Js.log 
;; f15 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 [@bs] |. Js.log 
;; f16 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 [@bs] |. Js.log 
;; f17 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 [@bs] |. Js.log 
;; f18 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 [@bs] |. Js.log 
;; f19 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 [@bs] |. Js.log 
;; f20 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 [@bs] |. Js.log 
;; f21 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 [@bs] |. Js.log 
;; f22 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 [@bs] |. Js.log 