let f re = 
  let _ = re |. Js.Re.exec_ "banana" in 
  3