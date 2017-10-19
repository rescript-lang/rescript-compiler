
let focus id =
  Tea_cmd.call (fun _enqueue -> match Js.Null_undefined.to_opt (Web.Document.getElementById id) with
      | None -> Js.log ("Attempted to focus a non-existant element of: ", id)
      | Some elem ->
        (* let () = Js.log ("Focusing element", id, elem) in *)
        let ecb _ignored = Web.Node.focus elem in
        let cb _ignored = let _unhandledID = Web.Window.requestAnimationFrame ecb in () in (* One to get out of the current render frame*)
        let _unhandledID = Web.Window.requestAnimationFrame cb in (* And another to properly focus *)
        ()
    )
