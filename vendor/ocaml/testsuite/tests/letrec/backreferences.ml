(* testing backreferences; some compilation scheme may handle
   differently recursive references to a mutually-recursive RHS
   depending on whether it is before or after in the bindings list *)
type t = { x : t; y : t; z : t }

let test =
  let rec x = { x; y; z }
      and y = { x; y; z }
      and z = { x; y; z }
  in
  List.iter (fun (f, t_ref) ->
    List.iter (fun t -> assert (f t == t_ref)) [x; y; z]
  )
    [
      (fun t -> t.x), x;
      (fun t -> t.y), y;
      (fun t -> t.z), z;
    ]
