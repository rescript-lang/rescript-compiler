(* random chunk size: 2^15, TODO: why do we guess randomly? *)
let chunkSize = 32768

let readFile ~filename =
  let chan = open_in filename in
  let buffer = Buffer.create chunkSize in
  let chunk = (Bytes.create [@doesNotRaise]) chunkSize in
  let rec loop () =
    let len = try input chan chunk 0 chunkSize with Invalid_argument _ -> 0 in
    if len == 0 then (
      close_in_noerr chan;
      Buffer.contents buffer
    ) else (
      Buffer.add_subbytes buffer chunk 0 len;
      loop ()
    )
  in
  loop ()

let readStdin () =
  let buffer = Buffer.create chunkSize in
  let chunk = (Bytes.create [@doesNotRaise]) chunkSize in
  let rec loop () =
    let len = try input stdin chunk 0 chunkSize with Invalid_argument _ -> 0 in
    if len == 0 then (
      close_in_noerr stdin;
      Buffer.contents buffer
    ) else (
      Buffer.add_subbytes buffer chunk 0 len;
      loop ()
    )
  in
  loop ()

let writeFile ~filename ~content =
  let chan = open_out_bin filename in
  output_string chan content;
  close_out chan
[@@raises Sys_error]
