let eliminate_intermediate_float_record () =
  let r = ref 0. in
  for n = 1 to 1000 do
    let open Complex in
    let c = { re = float n; im = 0. } in
    r := !r +. (norm [@inlined]) ((add [@inlined]) c i);
  done;
  ignore (Sys.opaque_identity !r)

