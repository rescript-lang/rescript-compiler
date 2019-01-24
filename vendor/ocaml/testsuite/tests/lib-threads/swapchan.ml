open Event

type 'a swap_chan = ('a * 'a channel) channel

let swap msg_out ch =
  guard (fun () ->
    let ic = new_channel() in
    choose [
      wrap (receive ch) (fun (msg_in, oc) -> sync (send oc msg_out); msg_in);
      wrap (send ch (msg_out, ic)) (fun () -> sync (receive ic))
    ])

let ch = new_channel()

let f () =
  let res = sync (swap "F" ch) in
  print_string "f "; print_string res; print_newline()

let g () =
  let res = sync (swap "G" ch) in
  print_string "g "; print_string res; print_newline()

let _ =
  let id = Thread.create f () in
  g ();
  Thread.join id
