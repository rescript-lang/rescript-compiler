let r = ref 0

let true_effect () =
  incr r;
  print_int !r; print_char ' ';
  true

let false_effect () =
  incr r;
  print_int !r; print_char ' ';
  false

let test i f =
  print_int i;
  print_string ": ";
  print_endline (string_of_bool (f ()))

let s = Bytes.of_string "\000"
let () =
  (* ensure that the string is not constant *)
  Bytes.set s 0 '\001'

let unknown_true =
  Bytes.get s 0 = '\001'

let unknown_false =
  Bytes.get s 0 <> '\001'

let () =
  test 1 (fun () -> true || true);
  test 2 (fun () -> true || false);
  test 3 (fun () -> true || true_effect ());
  test 4 (fun () -> true || false_effect ());
  test 5 (fun () -> true || unknown_true);
  test 6 (fun () -> true || unknown_false);
  test 7 (fun () -> false || true);
  test 8 (fun () -> false || false);
  test 9 (fun () -> false || true_effect ());
  test 10 (fun () -> false || false_effect ());
  test 11 (fun () -> false || unknown_true);
  test 12 (fun () -> false || unknown_false);
  test 13 (fun () -> true_effect () || true);
  test 14 (fun () -> true_effect () || false);
  test 15 (fun () -> true_effect () || true_effect ());
  test 16 (fun () -> true_effect () || false_effect ());
  test 17 (fun () -> true_effect () || unknown_true);
  test 18 (fun () -> true_effect () || unknown_false);
  test 19 (fun () -> false_effect () || true);
  test 20 (fun () -> false_effect () || false);
  test 21 (fun () -> false_effect () || true_effect ());
  test 22 (fun () -> false_effect () || false_effect ());
  test 23 (fun () -> false_effect () || unknown_true);
  test 24 (fun () -> false_effect () || unknown_false);
  test 25 (fun () -> unknown_true || true);
  test 26 (fun () -> unknown_true || false);
  test 27 (fun () -> unknown_true || true_effect ());
  test 28 (fun () -> unknown_true || false_effect ());
  test 29 (fun () -> unknown_true || unknown_true);
  test 30 (fun () -> unknown_true || unknown_false);
  test 31 (fun () -> unknown_false || true);
  test 32 (fun () -> unknown_false || false);
  test 33 (fun () -> unknown_false || true_effect ());
  test 34 (fun () -> unknown_false || false_effect ());
  test 35 (fun () -> unknown_false || unknown_true);
  test 36 (fun () -> unknown_false || unknown_false);
  test 37 (fun () -> true && true);
  test 38 (fun () -> true && false);
  test 39 (fun () -> true && true_effect ());
  test 40 (fun () -> true && false_effect ());
  test 41 (fun () -> true && unknown_true);
  test 42 (fun () -> true && unknown_false);
  test 43 (fun () -> false && true);
  test 44 (fun () -> false && false);
  test 45 (fun () -> false && true_effect ());
  test 46 (fun () -> false && false_effect ());
  test 47 (fun () -> false && unknown_true);
  test 48 (fun () -> false && unknown_false);
  test 49 (fun () -> true_effect () && true);
  test 50 (fun () -> true_effect () && false);
  test 51 (fun () -> true_effect () && true_effect ());
  test 52 (fun () -> true_effect () && false_effect ());
  test 53 (fun () -> true_effect () && unknown_true);
  test 54 (fun () -> true_effect () && unknown_false);
  test 55 (fun () -> false_effect () && true);
  test 56 (fun () -> false_effect () && false);
  test 57 (fun () -> false_effect () && true_effect ());
  test 58 (fun () -> false_effect () && false_effect ());
  test 59 (fun () -> false_effect () && unknown_true);
  test 60 (fun () -> false_effect () && unknown_false);
  test 61 (fun () -> unknown_true && true);
  test 62 (fun () -> unknown_true && false);
  test 63 (fun () -> unknown_true && true_effect ());
  test 64 (fun () -> unknown_true && false_effect ());
  test 65 (fun () -> unknown_true && unknown_true);
  test 66 (fun () -> unknown_true && unknown_false);
  test 67 (fun () -> unknown_false && true);
  test 68 (fun () -> unknown_false && false);
  test 69 (fun () -> unknown_false && true_effect ());
  test 70 (fun () -> unknown_false && false_effect ());
  test 71 (fun () -> unknown_false && unknown_true);
  test 72 (fun () -> unknown_false && unknown_false);
  ()

(* test generation *)

(*
let values = ["true"; "false"; "true_effect ()"; "false_effect ()";
              "unknown_true"; "unknown_false"]
let ops = ["||"; "&&"]
let count = ref 0
let f op v1 v2 =
  incr count;
  Printf.sprintf "  test %i (fun () -> %s %s %s);" !count v1 op v2

let s =
  List.iter (fun op ->
      List.iter (fun v1 ->
          List.iter (fun v2 -> print_endline (f op v1 v2))
            values)
        values)
    ops
*)
