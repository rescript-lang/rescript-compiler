(* Standard test case *)
let () =
  let l = List.init 10 (fun x -> x) in
  assert (List.exists (fun a -> a < 10) l);
  assert (List.exists (fun a -> a > 0) l);
  assert (List.exists (fun a -> a = 0) l);
  assert (List.exists (fun a -> a = 1) l);
  assert (List.exists (fun a -> a = 2) l);
  assert (List.exists (fun a -> a = 3) l);
  assert (List.exists (fun a -> a = 4) l);
  assert (List.exists (fun a -> a = 5) l);
  assert (List.exists (fun a -> a = 6) l);
  assert (List.exists (fun a -> a = 7) l);
  assert (List.exists (fun a -> a = 8) l);
  assert (List.exists (fun a -> a = 9) l);
  assert (not (List.exists (fun a -> a < 0) l));
  assert (not (List.exists (fun a -> a > 9) l));
  assert (List.exists (fun _ -> true) l);

  assert (List.compare_lengths [] [] = 0);
  assert (List.compare_lengths [1;2] ['a';'b'] = 0);
  assert (List.compare_lengths [] [1;2] < 0);
  assert (List.compare_lengths ['a'] [1;2] < 0);
  assert (List.compare_lengths [1;2] [] > 0);
  assert (List.compare_lengths [1;2] ['a'] > 0);

  assert (List.compare_length_with [] 0 = 0);
  assert (List.compare_length_with [] 1 < 0);
  assert (List.compare_length_with [] (-1) > 0);
  assert (List.compare_length_with [] max_int < 0);
  assert (List.compare_length_with [] min_int > 0);
  assert (List.compare_length_with [1] 0 > 0);
  assert (List.compare_length_with ['1'] 1 = 0);
  assert (List.compare_length_with ['1'] 2 < 0);
  ()
;;

(* Empty test case *)
let () =
  assert ((List.init 0 (fun x -> x)) = []);
;;

(* Erroneous test case *)

let () =
  let result = try
      let _ = List.init (-1) (fun x -> x) in false
  with Invalid_argument e -> true (* Exception caught *)
  in assert result;
;;

(* Evaluation order *)
let () =
  let test n =
    let result = ref false in
    let _ = List.init n (fun x -> result := (x = n - 1)) in
    assert !result
  in
  let threshold = 10_000 in (* Threshold must equal the value in stdlib/list.ml *)
  test threshold; (* Non tail-recursive case *)
  test (threshold + 1) (* Tail-recursive case *)
;;

let () = print_endline "OK";;
