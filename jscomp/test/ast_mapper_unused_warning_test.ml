


type t = 
   [`A of int 
   |`B of string]
   [@@bs.deriving jsConverter]