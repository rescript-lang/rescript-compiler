
let count = 1_000_000 


let bench () = 
  let data = ref Bs.SetInt.empty in 
  [%time for i = 0 to count do 
    data := 
      Bs.SetInt.add i  !data 
  done] ;
  [%time for i = 0 to count do  
    assert (Bs.SetInt.mem i !data)
  done]; 
  [%time for i = 0 to count do 
    data := Bs.SetInt.remove i !data  
  done ];
  assert  (Bs.SetInt.length !data = 0)



;;  [%time bench ()]

  
