

let rec searchAux i (xs : (int * _) array) (k : int) =  
  let (a,b) = Array.unsafe_get xs i in 
  if a = k then b 
  else searchAux (succ i) xs k 

let searchForSureExists xs k =   
  searchAux 0 xs k 


let search (id : int) array =   
  searchAux 0 array id 

let rec revSearchAux 
    i len (xs : (int * string) array) (k : string) = 
  if i = len then None 
  else 
    let (idx,s) = Array.unsafe_get xs i  in 
    if s = k then 
      Some idx 
    else 
      revSearchAux (i + 1) len xs k 

let revSearch len array (x : string)  : int option =  
   (revSearchAux 0 len array x)

let toInt (i : int) (xs : int array) =   
  Array.unsafe_get xs i

let rec fromIntAux (enum : int) i len xs = 
  if i = len then None
  else 
    let k = Array.unsafe_get xs i in 
    if k = enum then Some i 
    else fromIntAux enum (i + 1) len xs 

let fromInt len (xs : int array) (enum : int )  : 'variant option =   
    fromIntAux enum 0 len xs 