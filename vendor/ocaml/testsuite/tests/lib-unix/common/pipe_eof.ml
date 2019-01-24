let drain pipe =
  let max = 2048 in
  let buf = Buffer.create 2048 in
  let tmp = Bytes.create max in
  while begin
    try
      let len = Unix.read pipe tmp 0 max in
      Buffer.add_subbytes buf tmp 0 len;
      len > 0
    with Unix.Unix_error (Unix.EPIPE, _, _) when false ->
      false
  end do () done;
  Buffer.contents buf
;;

let run exe args =
  let out_in, out_out = Unix.pipe () in
  let err_in, err_out = Unix.pipe () in
  let args = Array.append [| exe |] args in
  let pid = Unix.create_process exe args Unix.stdin out_out err_out in
  Unix.close out_out;
  Unix.close err_out;
  let output = drain out_in in
  let error = drain err_in in
  Unix.close out_in;
  Unix.close err_in;
  let _pid, status = Unix.waitpid [ ] pid in
  status, output, error
;;

let _ =
  ignore (run "cp" [||]);
  print_endline "success"
;;
