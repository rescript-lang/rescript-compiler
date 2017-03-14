

let left_pad = Printf.sprintf ("%*s")

let () = 
  let left_pad_size = 32 in
	left_pad left_pad_size "Hello, World!" |> Js.log
