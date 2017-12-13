
let count = 1_000_000
let bench () = 
  let table = Hashtbl.create 1_000_000 in 
  for i = 0 to count do 
    Hashtbl.add table i i 
  done ;
  for i = 0 to count do 
    assert (Hashtbl.mem table i)
  done ;
  for i = 0 to count do 
    Hashtbl.remove table i 
  done 

;; bench ()