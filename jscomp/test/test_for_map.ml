module IntMap = Map.Make (struct
  type t = int

  let compare (x : int) y = compare x y
end)

let assertion_test () =
  let m = ref IntMap.empty in
  let count = 1000000 in
  for i = 0 to count do
    m := IntMap.add i i !m
  done ;
  for i = 0 to count do
    ignore (IntMap.find i !m)
  done
