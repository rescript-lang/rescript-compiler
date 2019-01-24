open Printf

(* Regression test for PR#4466: select timeout with simultaneous read
   and write on socket in Windows. *)

(* Scenario:
     - thread [server] implements a simple 'echo' server on a socket
     - thread [reader] selects then reads from a socket connected to
       the echo server and copies to standard output
     - main program executes [writer], which writes to the same socket
       (the one connected to the echo server)
*)

let serve_connection s =
  let buf = Bytes.make 1024 '>' in
  while true do
    let n = Unix.recv s buf 2 (Bytes.length buf - 2) [] in
    if n = 0 then begin
      Unix.close s; Thread.exit ()
    end else begin
      ignore (Unix.send s buf 0 (n + 2) [])
    end
  done

let server sock =
  while true do
    let (s, _) = Unix.accept sock in
    ignore(Thread.create serve_connection s)
  done

let reader s =
  let buf = Bytes.make 16 ' ' in
  match Unix.select [s] [] [] 10.0 with
  | (_::_, _, _) ->
      printf "Selected\n%!";
      let n = Unix.recv s buf 0 (Bytes.length buf) [] in
      printf "Data read: %s\n%!" (Bytes.sub_string buf 0 n)
  | ([], _, _) ->
      printf "TIMEOUT\n%!"

let writer s msg =
  ignore (Unix.send_substring s msg 0 (String.length msg) [])

let _ =
  let addr = Unix.ADDR_INET(Unix.inet_addr_loopback, 0) in
  let serv =
    Unix.socket (Unix.domain_of_sockaddr addr) Unix.SOCK_STREAM 0 in
  Unix.setsockopt serv Unix.SO_REUSEADDR true;
  Unix.bind serv addr;
  let addr = Unix.getsockname serv in
  Unix.listen serv 5;
  ignore (Thread.create server serv);
  Thread.delay 0.2;
  let client =
    Unix.socket (Unix.domain_of_sockaddr addr) Unix.SOCK_STREAM 0 in
  Unix.connect client addr;
  (* Send before select & read *)
  writer client "1111";
  let a = Thread.create reader client in
  Thread.delay 0.1;
  Thread.join a;
  (* Select then send *)
  let a = Thread.create reader client in
  Thread.delay 0.1;
  writer client "2222";
  Thread.join a;
  (* Select then send again *)
  let a = Thread.create reader client in
  Thread.delay 0.1;
  writer client "3333";
  Thread.join a
