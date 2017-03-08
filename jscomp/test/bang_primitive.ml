

(*
external log : 'a -> unit = "#console.log"

external log2 : 'a -> unit = "#console.log"


let f x = 
    log x;
    log2 x  
    *)


external xx : 'a -> string = "#anything_to_string" 


let test x =
    xx x 