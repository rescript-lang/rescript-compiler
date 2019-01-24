(* PR#6136 *)

exception Ok

let first () =
  let f g x = ignore (failwith "called f"); g in
  let g x = x in
  f g 2 (raise Ok)

let second () =
  let f g x = ignore (failwith "called f"); g in
  let g x = x in
  let h f = f g 2 (raise Ok) in
  ignore (h f)

let () =
  try
    ignore (first ());
    assert false
  with Ok ->
    try
      ignore (second ());
      assert false
    with Ok -> ()
