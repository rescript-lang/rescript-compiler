

val js_obj :
   <
         bark :  ('a ->  int ->  int -> int [@bs.this]) ;
         length : int; 
         x : int;
         y : int
       > Js.t
       as 'a 

val uux_this : 
  < length : int > Js.t ->  int -> int ->  int [@bs.this]
