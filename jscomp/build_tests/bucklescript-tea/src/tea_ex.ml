
(* Everything here is not in Elm and is purely used as an extension and may vanish at any time if a better API comes out. *)

module LocalStorage = struct
  open Tea_task
  open Tea_result

  let length =
    nativeBinding (fun cb ->
        match Web.Window.LocalStorage.length Web.Window.window with
        | None -> cb (Error "localStorage is not available")
        | Some value -> cb (Ok value)
      )

  let clear =
    nativeBinding (fun cb ->
        match Web.Window.LocalStorage.clear Web.Window.window with
        | None -> cb (Error "localStorage is not available")
        | Some value -> cb (Ok value)
      )
  let clearCmd () = Tea_task.attemptOpt (fun _ -> None) clear

  let key idx =
    nativeBinding (fun cb ->
        match Web.Window.LocalStorage.key Web.Window.window idx with
        | None -> cb (Error "localStorage is not available")
        | Some value -> cb (Ok value)
      )

  let getItem key =
    nativeBinding (fun cb ->
        match Web.Window.LocalStorage.getItem Web.Window.window key with
        | None -> cb (Error "localStorage is not available")
        | Some value -> cb (Ok value)
      )

  let removeItem key =
    nativeBinding (fun cb ->
        match Web.Window.LocalStorage.removeItem Web.Window.window key with
        | None -> cb (Error "localStorage is not available")
        | Some value -> cb (Ok value)
      )
  let removeItemCmd key = Tea_task.attemptOpt (fun _ -> None) (removeItem key)

  let setItem key value =
    nativeBinding (fun cb ->
        match Web.Window.LocalStorage.setItem Web.Window.window key value with
        | None -> cb (Error "localStorage is not available")
        | Some () -> cb (Ok ())
      )
  let setItemCmd key value = Tea_task.attemptOpt (fun _ -> None) (setItem key value)
end
