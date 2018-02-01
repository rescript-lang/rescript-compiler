
let count = 1_000_000 

module N = Bs.Set.Int
let bench () = 
  let data = ref N.empty in 
  [%time for i = 0 to count do 
    data := 
      N.add  !data i
  done] ;
  [%time for i = 0 to count do  
    assert (N.has !data i)
  done]; 
  [%time for i = 0 to count do 
    data := N.remove !data i 
  done ];
  assert  (N.size !data = 0)



;;  [%time bench ()]

  
