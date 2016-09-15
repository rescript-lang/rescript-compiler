external joinClasses : int array  -> string = ""
  [@@bs.module] [@@bs.splice]


let f () =
  joinClasses [|1;2;3|]
