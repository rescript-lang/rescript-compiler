let f x y = Js.(unsafe_lt x y, unsafe_le x y, unsafe_gt x y, unsafe_ge x y)
let ff x y = if Js.unsafe_lt x y then 1 else 2
