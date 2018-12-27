

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
#if OCAML_VERSION =~ "<4.03.0" then (* recursive lhs not allowed *)
let rec xs = 
  let rec ys = 1 :: ys 
  and _zs () = (List.hd ys, List.hd (fst xs)) in
  (2 :: List.hd ys :: [], _zs)
#end
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

type u = 
  | B of string * (unit -> u)
  | A of int * (unit -> u)

let rec rec_variant_b = 
    B ("gho", (fun _ -> rec_variant_a))

and rec_variant_a = 
    A (3, fun _ -> rec_variant_b)    


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
  __LOC__, (fun _ -> 
    Eq([1;2], fake_v)
  );
  __LOC__, (fun _ -> 
    Eq([2;3], fake_y)
  );
  __LOC__ , (fun _ -> 
    Eq (( [1;2;3]),( fake_z))
  );
  __LOC__, (fun _ -> 
    Eq ([1;55;2;3], fake_z2)
  );
  __LOC__, (fun _ -> 
    Eq ([2;3], fake_y2)
  );
  __LOC__, (fun _ -> 
    Eq ((  3), ( v)));

  __LOC__, (fun _ -> 
    match rec_variant_b with 
    | B(_,f) -> Eq(f () , rec_variant_a)
    | _ -> assert false 
  );    
  __LOC__, (fun _ -> 
    match rec_variant_a with 
    | A(_,f) -> Eq(f () , rec_variant_b)
    | _ -> assert false 
  );    
]


let rec
fake_odd n = fake_minus n 
and fake_minus n =
  Js.log n; 
  n + 1
  
let rec 
fake_inline n = fake_inline_minus n 
and fake_inline_minus n = 
   n + 1


let fake_inline_inlie2 = fake_inline_minus 3 
 ;; Mt.from_pair_suites __MODULE__ suites 


