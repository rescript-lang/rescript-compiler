

open Js.Internal

let f x = 
  !x # height + !x # width 

let g x : unit  = 
  let () = Js.Internal.fn_run1 !x # method1 3 in
  Js.Internal.fn_run2 !x # method2 3 3

class type _metric = object  method height : int [@@bs.set] method width : int [@@bs.set] end[@bs] 
let h x : unit  = 
  x##height #= 3 ; 
  x##width #= 3 
(* can not write set api without syntax extension, any more
*)

(**
imagine you have 
   {[
     let h = x ##_set_height in 
     h 3 (* this does not make sense *)
   ]}

the type should be 
   {[
       < _set_height : int -> unit> Js.t     
   ]}
or 
   {[
     < _set_height : (int, unit) t > Js.t
   ]}   

so {[
     x #. height ;
     x ## _set_height 3;
     x ## hey (1,2,3);
     x ## hey [| 1;2;3 |]
   ]}

we can not use tuple since ambiguity (there is no single tuple), 
list or array is okay, prefer array to list 

other edge cases 
   {[ x ## hey A.[| 1;2;3;4 |] ]}

so x will be of type 
   {[
     < _set_height : int -> unit ,
       hey : (int * int * int * unit) t ;
     ..     
     >     
   ]}

For the invariant, we can force [_set_*]

also try to make it still work for 
   {[
     x # _set_height 3 
   ]}

we introduce special syntax for property ready is mainly to distinguish
such cases,
if it is already a property read, there ? how about 

   {[ x #.property_fun 1 2   ]} 
this is mostly an error in FFI binding
*)
