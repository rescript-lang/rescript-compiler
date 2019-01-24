let x = ref 0

let () =
  Api.reg_mod "Plugin_ref";

  Api.add_cb
    (fun () ->
       Printf.printf "current value for ref = %i\n" !x;
       incr x
    )
