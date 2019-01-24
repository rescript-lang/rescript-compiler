(* Producer-consumer with events and multiple producers *)

open Event

let rec produce chan n max =
  sync (send chan n);
  if n < max then produce chan (n + 1) max else sync (send chan (-1))

let rec consume chans sum =
  let rec mkreceive prev = function
    | [] -> []
    | chan :: rem as chans ->
        wrap (receive chan) (fun n ->
          if n < 0
          then consume (List.rev_append rem prev) sum
          else consume (List.rev_append chans prev) (sum + n))
        :: mkreceive (chan :: prev) rem
  in
    if chans = [] then sum else select (mkreceive [] chans)

let sum_0_n n = n * (n + 1) / 2

let _ =
  let chan1 = new_channel()
  and chan2 = new_channel()
  and chan3 = new_channel() in
  ignore (Thread.create (fun () -> produce chan1 0 5000) ());
  ignore (Thread.create (fun () -> produce chan2 0 2000) ());
  ignore (Thread.create (fun () -> produce chan3 0 1000) ());
  let n = consume [chan1; chan2; chan3] 0 in
  if n = sum_0_n 5000 + sum_0_n 2000 + sum_0_n 1000
  then print_string "passed\n"
  else print_string "FAILED\n"
