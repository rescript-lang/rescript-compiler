

  match [%node __filename] with
  | Some f ->
    Node.Fs2.readFileSync f `utf8 |> Js.log
  | None -> ()
