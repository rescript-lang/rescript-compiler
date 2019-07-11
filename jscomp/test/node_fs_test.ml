;;
match [%node __filename] with
| Some f -> Node.Fs.readFileSync f `utf8 |> Js.log
| None -> ()
