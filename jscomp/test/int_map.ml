include Map.Make (struct
  type t = int

  let compare (x : int) y = compare x y
end)
