open Printf

(* Threads, sockets, and buffered I/O channels *)
(* Serves as a regression test for PR#5578 *)

let serve_connection s =
  let ic = Unix.in_channel_of_descr s
  and oc = Unix.out_channel_of_descr s in
  let l = input_line ic in
  fprintf oc ">>%s\n" l;
  close_out oc

let server sock =
  while true do
    let (s, _) = Unix.accept sock in
    ignore(Thread.create serve_connection s)
  done

let client (addr, msg) =
  let sock =
    Unix.socket (Unix.domain_of_sockaddr addr) Unix.SOCK_STREAM 0 in
  Unix.connect sock addr;
  let ic = Unix.in_channel_of_descr sock
  and oc = Unix.out_channel_of_descr sock in
  output_string oc msg; flush oc;
  let l = input_line ic in
  printf "%s\n%!" l

let _ =
  let addr = Unix.ADDR_INET(Unix.inet_addr_loopback, 0) in
  let sock =
    Unix.socket (Unix.domain_of_sockaddr addr) Unix.SOCK_STREAM 0 in
  Unix.setsockopt sock Unix.SO_REUSEADDR true;
  Unix.bind sock addr;
  let addr = Unix.getsockname sock in
  Unix.listen sock 5;
  ignore (Thread.create server sock);
  ignore (Thread.create client (addr, "Client #1\n"));
  Thread.delay 0.5;
  client (addr, "Client #2\n")
