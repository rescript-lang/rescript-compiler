let _ =
  let a = [| 0.0; -. 0.0 |] in
  Printf.printf "%Lx %Lx\n"
                (Int64.bits_of_float a.(0)) (Int64.bits_of_float a.(1))
