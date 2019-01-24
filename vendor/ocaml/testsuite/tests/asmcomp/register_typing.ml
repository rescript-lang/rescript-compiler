type 'a typ = Int : int typ | Ptr : int list typ

let f (type a) (t : a typ) (p : int list) : a =
  match t with
  | Int -> 100
  | Ptr -> p

let allocate_garbage () =
  for i = 0 to 100 do
    ignore (Array.make 200 0.0)
  done

let g (t : int list typ) x =
  Gc.minor ();
  let x = f t ([x; x; x; x; x]) in
  Gc.minor ();
  allocate_garbage ();
  ignore (String.length (String.concat " " (List.map string_of_int x)))

let () = g Ptr 5
