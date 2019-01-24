module Float_record : sig
  type t = private float;;

  val make : float -> t;;
  val from : t -> float;;

  type s = {f : t};;
end = struct
  type t = float;;

  let make f = f;;

  let from t = t;;

  type s = {f : t};;
end

module Float_array = struct
  let small_float_array x =
    [|1.;2.;3.|], x

  let longer_float_array x =
    [|1.;2.;3.;4.;5.;6.;7.;8.;9.;0.;
      1.;2.;3.;4.;5.;6.;7.;8.;9.;0.;
      1.;2.;3.;4.;5.;6.;7.;8.;9.;0.;
      1.;2.;3.;4.;5.;6.;7.;8.;9.;0.;|], x
end

let s = { Float_record.f = Float_record.make 1.0 };;

print_float (Float_record.from s.Float_record.f);;
print_newline ();;


let b = Float_array.small_float_array 12
let c = (Float_array.longer_float_array [@inlined]) 34

let print_array a =
  Array.iter (fun f ->
      print_float f;
      print_newline ()) a;
  print_newline ()

let () =
  print_array (fst b);
  print_array (fst c);
