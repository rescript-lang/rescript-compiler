type t = A | B
  [@@mli]

and s = C | D
  [@@mli abstract]


module X = struct
  type t = X | Y
  [@@mli]
  and s

  let id x = x
    [@@mli]
end

module Y : sig type t type s end = struct
  type t = X | Y
  type s = A | B
end

let f x y = x + y
    [@@mli]
and g a b = (a, b)
    [@@mli]
and h a b = (a, b)
    [@@mli (h : int -> int -> int * int)]

let (x, y, z) = (1, 2, 3)
    [@@mli (x : int), (y : int)]
