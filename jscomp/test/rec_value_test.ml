

let rec x = 1::x

let rec a = 2::b 
and b = 3 :: c 
and c = 3 :: a 

let rec xx = 
  let v = 1 in v :: xx

let rec naive = 
  let uu = naive in
  function 
    | 0 | 1 -> 1 
    | n -> n + uu (n-1) + uu (n - 2)
let rec fib = 
  let one = 1 in
  let four = ref 2 in
  let three = ref 3 in
  let u = fib in
  let h = lazy fib in
  let v = ref (fun _ -> assert false) in
  function 
    | 0  -> !four
    | 1 -> one 
    | 2 -> !three
    | 3 -> 
        v := Lazy.force h ;
        one
    | n -> fib (n - 1) + u (n - 2)

let rec xs = 
  let rec ys = 1 :: ys 
  and _zs () = (List.hd ys, List.hd (fst xs)) in
  (2 :: List.hd ys :: [], _zs)

let rec xs = 
  let zs () = ( List.hd (fst xs)) in
  (2 :: [], zs)

let rec fib2 = 
  let _one = (fun _ -> 1 + two)  in
  function | 0 | 1 -> 1 | n -> fib2 (n - 1) + fib2 (n - 2)
and two = 2 


let rec fib3 = 
  let _one = (fun _ -> 1 + two)  in
  function | 0 | 1 -> 1 | n -> fib3 (n - 1) + fib3 (n - 2)
(* and fib4 = fib3 *) (* not allowed *)

let rec even = 
  let odd n =  if n ==1 then true else even (n - 1) in
  fun n -> if n ==0  then true else odd (n - 1)


let rec even2 = 
  (* let _b = even2 0 in *)
  let odd = even2 in
  fun n -> if n ==0  then true else odd (n - 1)

let rec lazy_v = lazy (fun _ -> ignore @@ Lazy.force lazy_v)
let rec sum = 
  let a = sum in 
  fun acc n -> 
    if n > 0 then a (acc + n) (n - 1)
    else acc 

(* let rec v =  *)
(*   if sum 0 10 > 20 then  *)
(*     fun _ -> print_endline "hi"; v () *)
(*   else  *)
(*     fun _-> print_endline "hey"; v () *)

let rec fake_v = 1::2::[]

let rec fake_y = 2::3::[]
and fake_z = 1::fake_y

(** faked mutual recursive value, should be detected by [scc] *)
let rec fake_z2 = 1::(sum 0 10) :: fake_y2
and fake_y2 = 2::3::[]

let rec v = 3

let suites = Mt.[
  "hd", (fun _ -> 
    Eq(1, List.hd (List.tl x)));
  "mutual", (fun _ -> 
    Eq (3, 
        (match a with 
    |_ :: _ :: _ :: _ :: c :: _ -> c 
    | _ -> 
        
        (* 3333 *)
  assert false
        )));
  "rec_sum", (fun _ -> Eq(55, sum 0 10));
  "fake_rec", (fun _ -> 
    Eq (([1;2], [2;3], [1;2;3], [1;55;2;3], [2;3], 3), (fake_v, fake_y, fake_z, fake_z2, fake_y2,v)))
]

open Mt
;; from_pair_suites __FILE__ suites
