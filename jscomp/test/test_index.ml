





let f (x : < case : int ->  'a ; 
            case_set : int * int -> unit ;
            .. > [@uncurry] [@bs.obj])
 = 
  x ## case_set (3, 2) ;
  x ## case 3 

class type ['a] case = object [@uncurry]
  method case : int -> 'a 
  method case_set : int * 'a -> unit 
end

let ff (x : int case  Js.t)
 = 
  x##case_set (3, 2) ;
  x##case 3 



let h (x : 
         < case : (int ->  (int -> 'a ) ); .. >  [@uncurry] [@bs.obj]) = 
  let a = x##case 3 in 
  a 2 [@uncurry]   


type x_obj =  
  < 
    case : int ->  int ; 
    case_set : int * int -> unit ;
  >  [@uncurry] [@bs.obj]

let f_ext 
    (x : x_obj)
 = 
  x ## case_set (3, 2) ;
  x ## case 3 

type 'a h_obj = 
  < 
    case : int ->  (int -> 'a)
  > [@uncurry] [@bs.obj]

let h_ext  (x : 'a h_obj) = 
  let  a = x ##case 3 in 
  a 2 [@uncurry] 
