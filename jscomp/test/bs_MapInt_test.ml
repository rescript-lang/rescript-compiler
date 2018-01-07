let should b = 
  if not b then Js.Exn.raiseError "IMPOSSIBLE"
let test () =
  let m = ref Bs.MapInt.empty in
  let count = 100_0000 - 1 in
  for i = 0 to count do
    m := Bs.MapInt.add i i !m
  done;
  for i = 0 to count do
    should (Bs.MapInt.findOpt i !m <> None)
  done; 
  for i = 0 to count do 
    m := Bs.MapInt.remove i !m ;
  done ;
  should (Bs.MapInt.length !m = 0)

let () =
  test ()
