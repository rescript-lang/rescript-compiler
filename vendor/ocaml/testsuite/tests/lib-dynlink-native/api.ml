let mods = ref []

let reg_mod name =
  if List.mem name !mods then
    Printf.printf "Reloading module %s\n" name
  else (
    mods := name :: !mods;
    Printf.printf "Registering module %s\n" name
  )


let cbs = ref []

let add_cb f = cbs := f :: !cbs
let runall () = List.iter (fun f -> f ()) !cbs

(*
let () =
  at_exit runall
*)
