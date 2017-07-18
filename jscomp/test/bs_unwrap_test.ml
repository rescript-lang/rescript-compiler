external log1 :
  (
    [ `Pair of string * int
    | `Int of int
    | `String of string
    ] [@bs.unwrap]
  )
  -> unit = "console.log" [@@bs.val]

let _ = log1 (`Pair ("hello world", 1))
let _ = log1 (`Int 1337)
let _ = log1 (`String "hello world")

let arg_string = `String "hi runtime"
let _ = log1 arg_string

let arg_pair = `Pair ("hi", 1)
let _ = log1 arg_pair

external log2 :
  (
    [ `Unit of unit
    ] [@bs.unwrap]
  )
  -> unit = "console.log" [@@bs.val]

let _ = log2 (`Unit ())

external log3 :
  req:([ `String of string
       | `Int of int
       ] [@bs.unwrap])
  -> ?opt:([ `String of string
           ] [@bs.unwrap])
  -> unit
  -> unit = "console.log" [@@bs.val]

let _ = log3 ~req:(`Int 1) ()
let _ = log3 ~req:(`Int 1) ~opt:(`String "hi") ()

external log4 :
  ([ `String of string
   | `Options of [%bs.obj: < foo : int > ]
   ] [@bs.unwrap])
  -> unit = "console.log" [@@bs.val]

let _ = log4 (`String "foo")
let _ = log4 (`Options [%bs.obj { foo = 1 }])
