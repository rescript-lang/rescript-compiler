
let _ : _ Js.undefined = 
  Js.Undefined.bind [%node __filename] begin fun [@bs] f ->
    let content : string =
      Node.Fs.readFileSync  f `utf8 in 
    Js.log content 
  end
