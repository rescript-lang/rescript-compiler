let () =
  let delayed = ref (fun () -> ()) in
  for i = 1 to 2 do
    let rec f n = function
      | 0 -> assert (i = n)
      | j ->
          delayed :=
            let prev = !delayed in
            fun () ->
              prev () ;
              f (succ n + i - i) (pred j) in
    f 0 i
  done ;
  !delayed ()
