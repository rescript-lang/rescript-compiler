type t  =  
  | Atom of string 
  | List of t list
  | Data of t list 
  | Lit of string 


val token :  Lexing.lexbuf ->  t list

val from_file : string -> t list 

val from_string : string -> t list 