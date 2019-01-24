(* OCaml part of the code *)

let rec fib n =
  if n < 2 then 1 else fib(n-1) + fib(n-2)

let format_result n =
  let r = "Result = " ^ string_of_int n in
  (* Allocate gratuitously to test GC *)
  for i = 1 to 1500 do ignore (Bytes.create 256) done;
  r

(* Registration *)

let _ =
  Callback.register "fib" fib;
  Callback.register "format_result" format_result
