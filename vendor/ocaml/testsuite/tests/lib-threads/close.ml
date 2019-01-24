let main () =
  let (rd, wr) = Unix.pipe() in
  let t = Thread.create
    (fun () ->
      Thread.delay 1.0;
      print_endline "closing fd...";
      Unix.close wr;
    )
    () in
  let buf = Bytes.create 10 in
  print_endline "reading...";
  begin try ignore (Unix.read rd buf 0 10) with Unix.Unix_error _ -> () end;
  print_endline "read returned";
  t

let t = Unix.handle_unix_error main ()

let _ = Thread.join t
