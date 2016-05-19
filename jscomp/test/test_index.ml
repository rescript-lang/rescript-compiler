





let f (x : (< case : int ->  'a [@uncurry]; 
            case__set : int * int -> unit [@uncurry];
            .. > as 'b) Js.t)
 = 
  x ## case__set (3, 2) ;
  x ## case 3 

class type ['a] case = object 
  method case : int -> 'a [@uncurry]
  method case__set : int * 'a -> unit [@uncurry]
end

let ff (x : int case  Js.t)
 = 
  x ## case__set (3, 2) ;
  x ## case 3 



let h (x : (< case : (int ->  (int -> 'a [@uncurry]) [@uncurry]); .. > as 'b) Js.t) = 
  let a = x##case 3 in 
  a #@ 2   


type x_obj = [%uncurry: < 
       case : int ->  int ; 
       case__set : int * int -> unit ;
>  Js.t ]

let f_ext 
    (x : x_obj)
 = 
  x ## case__set (3, 2) ;
  x ## case 3 

type 'a h_obj = [%uncurry: < 
 case : int ->  (int -> 'a)
> Js.t ]

let h_ext  (x : 'a h_obj) = 
  let  a = x ##case 3 in 
  a #@ 2 
