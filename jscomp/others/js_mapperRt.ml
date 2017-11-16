

let rec searchAux i (xs : (int * _) array) (k : int) =  
  let (a,b) = Array.unsafe_get xs i in 
  if a = k then b 
  else searchAux (succ i) xs k 

let searchForSureExists xs k =   
  searchAux 0 xs k 


let search (x : 'poly_var) array = 
  let id : int = Obj.magic x  in 
  searchAux 0 array id 