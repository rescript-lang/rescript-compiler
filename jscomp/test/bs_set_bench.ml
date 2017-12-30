
let count = 1_000_000 


let bench () = 
  let data = ref Bs.SetInt.empty in 
  [%time for i = 0 to count do 
    data := 
      Bs.SetInt.add  !data i
  done] ;
  [%time for i = 0 to count do  
    assert (Bs.SetInt.mem !data i)
  done]; 
  [%time for i = 0 to count do 
    data := Bs.SetInt.remove !data i 
  done ];
  assert  (Bs.SetInt.length !data = 0)



;;  [%time bench ()]

  
