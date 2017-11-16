

let rec searchAux i (xs : (int * _) array) (k : int) =  
  let (a,b) = Array.unsafe_get xs i in 
  if a = k then b 
  else searchAux (succ i) xs k 

let searchForSureExists xs k =   
  searchAux 0 xs k 


let search (x : 'poly_var) array = 
  let id : int = Obj.magic x  in 
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

let revSearch len array (x : string)  : 'poly_var option =  

  Obj.magic (revSearchAux 0 len array x)

let toInt (i : 'enum) (xs : int array) =   
  Array.unsafe_get xs ( Obj.magic i : int)

let rec fromIntAux (enum : int) i len xs = 
  if i = len then None
  else 
    let k = Array.unsafe_get xs i in 
    if k = enum then Some i 
    else fromIntAux enum (i + 1) len xs 

let fromInt len (xs : int array) (enum : int )  : 'variant option =   
    (Obj.magic (fromIntAux enum 0 len xs ))