open Unix

external set_fake_clock : int64 -> unit = "set_fake_clock"

let real_time tm = {tm with tm_year = tm.tm_year + 1900; tm_mon = tm.tm_mon + 1}

let print_time () =
  let time = Unix.time () |> Unix.gmtime |> real_time in
  Printf.printf "System clock: %04d/%02d/%02d %02d:%02d\n" time.tm_year
                                                           time.tm_mon
                                                           time.tm_mday
                                                           time.tm_hour
                                                           time.tm_min

let test_mtime file =
  let time = (Unix.stat file).st_mtime |> Unix.gmtime |> real_time in
  Printf.printf "Read mtime for %s = %04d/%02d/%02d %02d:%02d:%02d\n"
    file
    time.tm_year time.tm_mon time.tm_mday time.tm_hour time.tm_min time.tm_sec

let _ =
  (* 1-Jun-2017 20:33:10.42+0000 *)
  set_fake_clock 0x1D2DB1648916FA0L;
  print_time ();
  test_mtime "dst-file";
  test_mtime "non-dst-file";
  (* 1-Feb-2017 20:33:10.42+0000 *)
  set_fake_clock 0x1D27CCA66FF6FA0L;
  print_time ();
  test_mtime "dst-file";
  test_mtime "non-dst-file"
