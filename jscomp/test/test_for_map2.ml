let assertion_test () =
  let m = ref Int_map.empty in
  let count = 1000000 in
  for i = 0 to count do
    m := Int_map.add i i !m
  done ;
  for i = 0 to count do
    ignore (Int_map.find i !m)
  done
