

let () = 
  Js.Console.timeStart "Int64.to_string";
  let u = Int64.(sub max_int 200_000L) in 
  for i = 0 to 1_000_000 do 
    Int64.to_string u |> ignore
  done   ;
  Js.Console.timeEnd "Int64.to_string"
