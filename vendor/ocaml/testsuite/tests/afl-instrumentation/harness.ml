external reset_instrumentation : bool -> unit = "caml_reset_afl_instrumentation"
external sys_exit : int -> 'a = "caml_sys_exit"

let name n =
  fst (Test.tests.(int_of_string n - 1))
let run n =
  snd (Test.tests.(int_of_string n - 1)) ()

let orig_random = Random.get_state ()

let () =
  (* Random.set_state orig_random; *)
  reset_instrumentation true;
  begin
    match Sys.argv with
    | [| _; "len" |] -> print_int (Array.length Test.tests); print_newline (); flush stdout
    | [| _; "name"; n |] -> print_string (name n); flush stdout
    | [| _; "1"; n |] -> run n
    | [| _; "2"; n |] -> run n; (* Random.set_state orig_random;  *)reset_instrumentation false; run n
    | _ -> failwith "error"
  end;
  sys_exit 0
