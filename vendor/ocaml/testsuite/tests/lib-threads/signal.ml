let sighandler _ =
  print_string "Got ctrl-C, exiting..."; print_newline();
  exit 0

let print_message delay c =
  while true do
    print_char c; flush stdout; Thread.delay delay
  done

let _ =
  ignore (Sys.signal Sys.sigint (Sys.Signal_handle sighandler));
  ignore (Thread.create (print_message 0.6666666666) 'a');
  print_message 1.0 'b'
