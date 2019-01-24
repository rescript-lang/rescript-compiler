open Lib;;
ignore (Gc.stat ());
let x = Array.make 20 "" in
let w = weak_create 20 in
for i = 0 to 19 do
  x.(i) <- String.make 20 's';
  weak_set w i (Some x.(i));
done;
Gc.full_major ();
for i = 0 to 19 do
  match weak_get w i with
  | None -> raise Not_found
  | _ -> ()
done;
for i = 0 to 19 do
  if i mod 2 = 0 then x.(i) <- ""
done;
Gc.full_major ();
for i = 0 to 19 do
  match weak_get w i with
  | None when i mod 2 = 0 -> ()
  | Some s when i mod 2 = 1 -> if s.[5] <> 's' then raise Not_found
  | _ -> raise Not_found
done
;;
