(* Torture test - I/O interspersed with lots of GC *)

let finished = ref false

let gc_thread () =
  while not !finished do
(*    print_string "gc"; print_newline(); *)
    Gc.minor();
    Thread.yield()
  done

let writer_thread (oc, size) =
  while not !finished do
(*    print_string "writer "; print_int size; print_newline(); *)
    let buff = Bytes.make size 'a' in
    ignore(Unix.write oc buff 0 size)
  done;
  let buff = Bytes.make size 'b' in
  ignore (Unix.write oc buff 0 size)

let reader_thread (ic, size) =
  while true do
(*    print_string "reader "; print_int size; print_newline(); *)
    let buff = Bytes.make size ' ' in
    let n = Unix.read ic buff 0 size in
(*    print_string "reader "; print_int n; print_newline(); *)
    for i = 0 to n-1 do
      if Bytes.get buff i = 'b' then Thread.exit()
      else if Bytes.get buff i <> 'a' then print_string "error in reader_thread\n"
    done
  done

let _ =
  let t1 = Thread.create gc_thread () in
  let (out1, in1) = Unix.pipe() in
  let t2 = Thread.create writer_thread (in1, 4096) in
  let t3 = Thread.create reader_thread (out1, 4096) in
  let (out2, in2) = Unix.pipe() in
  let t4 = Thread.create writer_thread (in2, 16) in
  let t5 = Thread.create reader_thread (out2, 16) in
  Thread.delay 3.0;
  finished := true;
  List.iter Thread.join [t1; t2; t3; t4; t5];
  print_string "passed\n"
