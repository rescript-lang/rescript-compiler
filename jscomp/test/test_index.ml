





let f (x : [%bs.obj: < case : int ->  'a ; 
            case_set : int * int -> unit ;
            .. > [@uncurry] ] )
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
         [%bs.obj:< case : (int ->  (int -> 'a ) ); .. >  [@uncurry] ]) = 
  let a = x##case 3 in 
  a 2 [@uncurry]   


type x_obj =  
  [%bs.obj: < 
    case : int ->  int ; 
    case_set : int * int -> unit ;
  >  [@uncurry] ]

let f_ext 
    (x : x_obj)
 = 
  x ## case_set (3, 2) ;
  x ## case 3 

type 'a h_obj = 
  [%bs.obj: < 
    case : int ->  (int -> 'a)
  > [@uncurry] ]

let h_ext  (x : 'a h_obj) = 
  let  a = x ##case 3 in 
  a 2 [@uncurry] 
