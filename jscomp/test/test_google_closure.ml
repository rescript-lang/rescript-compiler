let f a b _ = a + b
let f2 a = f a 1

let a, b, c =
  ( string_of_int (f 1 2 3)
  , (let f3 = f2 100 in
     f3 2)
  , let arr = Array.init 2 (fun _ -> 0) in
    for i = 0 to 1 do
      let f3 = f2 i in
      arr.(i) <- f3 2
    done ;
    arr )

let () = Js.log (a, b, c)
