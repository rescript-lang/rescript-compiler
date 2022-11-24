type 'a return = int -> 'a [@bs]

let h (x : 
         < cse : int -> 'a return [@bs] ; .. >   ) = 
   (x#@cse 3) 2 [@bs]



type x_obj =  
   < 
    cse : int ->  int [@bs] ; 
    cse__st : int -> int -> unit [@bs];
  > 

let f_ext 
    (x : x_obj)
 = 
 x #@ cse__st  3 2;
 x #@ cse  3


type 'a h_obj = 
  < 
    cse : int ->  'a return [@bs]
  > 

let h_ext  (x : 'a h_obj) = 
   (x #@cse 3) 2 [@bs]
