

type 'msg processMsg =
  | PushMsg of 'msg
  | Kill

let spawn initState update shutdown =
  let state = ref (Some initState) in
  let onMessage procMsg =
    match !state with
    | None -> ()
    | Some model ->
      ( match procMsg with
        | PushMsg msg ->
          let () = state := (update model msg) in
          ()
        | Kill ->
          let () = shutdown model in
          let () = state := None in
          ()
      ) in
  onMessage





(* let testing0 =
  let s = spawn 42 (fun model -> let () = Js.log model in function
      | `Inc -> Some (model + 1)
      | `Dec -> Some (model - 1)
    ) (fun _ -> ()) in
  let () = s (PushMsg `Dec) in
  let () = s (PushMsg `Dec) in
  let () = s Kill in
  let () = s (PushMsg `Dec) in
  () *)



module type Process = sig
  (* This module should be `import`ed into a module that will become a persistent process.
     That process should have a handleMsg callback to handle its own message types.
     It should call itself
  *)

  type msg

  val handleMsg : msg -> unit
end

let testing1 = 42
