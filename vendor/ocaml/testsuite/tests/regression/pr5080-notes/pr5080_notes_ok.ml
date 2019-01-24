let marshal_int f  =
  match [] with
  | _ :: `INT n :: _ -> f n
  | _ -> failwith "marshal_int"
