


let f x = 
    let unused_f x = Js.log "x" ; x + x in 
    let unused_h x = unused_f 3 in
    x + 3

module M (S : Set.S) : sig
    val f : int -> int  
end = struct
    let f x = x 
    let unused_g x = x + 2
    let unused_h x= unused_g x  
end    

let fff _ _ = 3 