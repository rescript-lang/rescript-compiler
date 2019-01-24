open Printf

(* Regression test for PR#5325: simultaneous read and write on socket
   in Windows. *)

(* Scenario:
     - thread [server] implements a simple 'echo' server on a socket
     - thread [reader] reads from a socket connected to the echo server
       and copies to standard output
     - main program executes [writer], which writes to the same socket
       (the one connected to the echo server)
     - thread [timeout] causes a failure if nothing happens in 10 seconds.
*)

let serve_connection s =
  let buf = Bytes.make 1024 '>' in
  let n = Unix.read s buf 2 (Bytes.length buf - 2) in
  ignore (Unix.write s buf 0 (n + 2));
  Unix.close s

let server sock =
  while true do
    let (s, _) = Unix.accept sock in
    ignore(Thread.create serve_connection s)
  done

let timeout () =
  Thread.delay 10.0;
  printf "Time out, exiting...\n%!";
  exit 2

let reader s =
  let buf = Bytes.make 1024 ' ' in
  let n = Unix.read s buf 0 (Bytes.length buf) in
  print_bytes (Bytes.sub buf 0 n); flush stdout

let writer s msg =
  ignore (Unix.write_substring s msg 0 (String.length msg));
  Unix.shutdown s Unix.SHUTDOWN_SEND

let _ =
  let addr = Unix.ADDR_INET(Unix.inet_addr_loopback, 0) in
  let serv =
    Unix.socket (Unix.domain_of_sockaddr addr) Unix.SOCK_STREAM 0 in
  Unix.setsockopt serv Unix.SO_REUSEADDR true;
  Unix.bind serv addr;
  let addr = Unix.getsockname serv in
  Unix.listen serv 5;
  ignore (Thread.create server serv);
  ignore (Thread.create timeout ());
  Thread.delay 0.5;
  let client =
    Unix.socket (Unix.domain_of_sockaddr addr) Unix.SOCK_STREAM 0 in
  Unix.connect client addr;
  let rd = Thread.create reader client in
  Thread.delay 0.5;
  writer client "Client data\n";
  Thread.join rd
