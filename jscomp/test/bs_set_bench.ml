
let count = 1_000_000 


let bench () = 
  let data = ref Bs.SetInt.empty in 
  for i = 0 to count do 
    data := 
      Bs.SetInt.add i  !data 
  done ;
  for i = 0 to count do  
    assert (Bs.SetInt.mem i !data)
  done; 
  for i = 0 to count do 
    data := Bs.SetInt.remove i !data  
  done ;
  assert  (Bs.SetInt.cardinal !data = 0)



;;  [%time bench ()]

  
