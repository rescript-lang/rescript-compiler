let should b = 
  if not b then Js.Exn.raiseError "IMPOSSIBLE"
let test () =
  let m = ref Bs.MapInt.empty in
  let count = 100_0000 - 1 in
  for i = 0 to count do
    m := Bs.MapInt.set !m i i 
  done;
  for i = 0 to count do
    should (Bs.MapInt.get !m i <> None)
  done; 
  for i = 0 to count do 
    m := Bs.MapInt.remove !m i  ;
  done ;
  should (Bs.MapInt.size !m = 0)

let () =
  test ()
