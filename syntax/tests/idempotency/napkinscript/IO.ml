module IO: sig
  val readFile: string -> string
  val readStdin: unit -> string
  val writeFile: string -> string -> unit
end = struct
  (* random chunk size: 2^15, TODO: why do we guess randomly? *)
  let chunkSize = 32768

  let readFile filename =
    let chan = open_in filename in
    let buffer = Buffer.create chunkSize in
    let chunk = Bytes.create chunkSize in
    let rec loop () =
      let len = input chan chunk 0 chunkSize in
      if len == 0 then (
        close_in chan;
        Buffer.contents buffer
      ) else (
        Buffer.add_subbytes buffer chunk 0 len;
        loop ()
      )
    in
    loop ()

  let readStdin () =
    let buffer = Buffer.create chunkSize in
    let chunk = Bytes.create chunkSize in
    let rec loop () =
      let len = input stdin chunk 0 chunkSize in
      if len == 0 then (
        close_in stdin;
        Buffer.contents buffer
      ) else (
        Buffer.add_subbytes buffer chunk 0 len;
        loop ()
      )
    in
    loop ()

  let writeFile filename txt =
    let chan = open_out_bin filename in
    output_string chan txt;
    close_out chan
end
