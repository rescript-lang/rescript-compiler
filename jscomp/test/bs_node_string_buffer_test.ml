let f str =
  match Node.test str with
  | Node.String, s -> Js.log ("string", s)
  | Node.Buffer, s -> Js.log ("buffer", Node.Buffer.isBuffer s)

let () =
  f [%bs.raw {|"xx"|}] ;
  f [%bs.raw {|Buffer.from ('xx')|}]
