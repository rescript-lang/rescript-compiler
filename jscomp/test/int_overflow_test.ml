let hash_variant s =
  let accu = ref 0 in
  for i = 0 to String.length s - 1 do
    accu := (223 * !accu + Char.code s.[i]) land (1 lsl 31 - 1)
    (* Here accu is 31 bits, times 223 will not be than 53 bits..
       TODO: we can use `Sys.backend_type` for patching
     *)
  done;
  (* reduce to 31 bits *)
  (* accu := !accu land (1 lsl 31 - 1); *)
  (* make it signed for 64 bits architectures *)
  if !accu > 0x3FFFFFFF then ( !accu - (1 lsl 31)) lor 0 else !accu


let hash_variant2 s =
  let accu = ref 0 in
  for i = 0 to String.length s - 1 do
    accu := 223 * !accu + Char.code s.[i]
  done;
  (* reduce to 31 bits *)
  accu := !accu land (1 lsl 31 - 1);
  (* make it signed for 64 bits architectures *)
  if !accu > 0x3FFFFFFF then !accu - (1 lsl 31) else !accu


let rec fib = function
  | 0l | 1l -> 1l
  | n -> Int32.add (fib (Int32.sub n  1l))  (fib  (Int32.sub n  2l))

;; Mt.from_pair_suites __FILE__ [
  "plus_overflow", (fun _ -> Eq(true, Int32.(add max_int 1l) = Int32.min_int) ) ;
  "minus_overflow", (fun _ -> Eq(true,  Int32.(sub min_int one ) = Int32.max_int));
  "flow_again", (fun _ ->  Eq ( 2147483646l, Int32.(add (add max_int max_int) min_int) ));
  "flow_again", (fun _ ->  Eq ( -2l, Int32.( (add max_int max_int) ) ));
  "hash_test", (fun _ -> Eq(hash_variant "xxyyzzuuxxzzyy00112233", 544087776));
  "hash_test2", (fun _ -> Eq(hash_variant "xxyyzxzzyy", -449896130) );
  __LOC__, (fun _ -> Eq(hash_variant2 "xxyyzzuuxxzzyy00112233", 544087776));
  __LOC__, (fun _ -> Eq(hash_variant2 "xxyyzxzzyy", -449896130) );

  "int_literal_flow", (fun _ -> Eq(-1, 0xffffffff) );
  "int_literal_flow2", (fun _ -> Eq(-1l, Int32.of_int 0xffffffff) );
  "int_literal_flow3", (fun _ -> Eq(-1l, Int32.of_int 0xfffffffff));
   "int32_mul", (fun _ -> Eq(-33554431l, Int32.mul 0xffffffl 0xffffffl )) ;
   __LOC__ , (fun _ -> Eq(int_of_float @@ Js.Float.fromString "3", 3)) ;
    (* FIXME *)
   __LOC__, (fun _ -> Eq (int_of_float @@ Js.Float.fromString "3.2", 3  ))
]
