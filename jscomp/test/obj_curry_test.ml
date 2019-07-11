let f o = o#hi 1 2 3

let a =
  f
    (object
       method hi x y z = x + y + z
    end)
