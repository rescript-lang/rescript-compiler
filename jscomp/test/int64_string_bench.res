let () = {
  Js.Console.timeStart("Int64.to_string")
  let u = {
    open Int64
    max_int->sub(200_000L)
  }
  /* let u = 3L in */
  for i in 0 to 1_00_000 {
    Int64.to_string(u) |> ignore
  }
  Js.Console.timeEnd("Int64.to_string")
  Js.log(Int64.to_string(u))
}
let () = {
  Js.Console.timeStart("Int64.to_string")
  let u = 30_000_000L
  for i in 0 to 1_00_000 {
    Int64.to_string(u) |> ignore
  }
  Js.Console.timeEnd("Int64.to_string")
  Js.log(Int64.to_string(u))
}

let () = {
  Js.Console.timeStart("Int64.to_string")
  let u = {
    open Int64
    min_int->add(100L)
  }
  for i in 0 to 1_00_000 {
    Int64.to_string(u) |> ignore
  }
  Js.Console.timeEnd("Int64.to_string")
  Js.log(Int64.to_string(u))
}
