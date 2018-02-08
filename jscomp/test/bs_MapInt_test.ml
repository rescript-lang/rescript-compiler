let should b = 
  if not b then Js.Exn.raiseError "IMPOSSIBLE"

module M = Belt.Map.Int  
let test () =
  let m = ref M.empty in
  let count = 100_0000 - 1 in
  for i = 0 to count do
    m := M.set !m i i 
  done;
  for i = 0 to count do
    should (M.get !m i <> None)
  done; 
  for i = 0 to count do 
    m := M.remove !m i  ;
  done ;
  should (M.isEmpty !m)

let () =
  test ()
