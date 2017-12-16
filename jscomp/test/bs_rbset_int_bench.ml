
let count = 1_000_000 

module V = Rbset 
let bench () = 
  let data = ref V.empty in 
  [%time for i = 0 to count do 
    data := 
      V.add i  !data 
  done] ;
  [%time for i = 0 to count do  
    assert (V.mem i !data)
  done]; 
  [%time for i = 0 to count do 
    data := V.remove i !data  
  done ] ;
  assert  (V.cardinal !data = 0)



;;  [%time bench ()]

  
