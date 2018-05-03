



module Make  ( S : sig type elt end ) = struct
    open S 
    type 'a t 
    external unsafe_get : elt t -> int -> elt = "" [@@bs.get_index]

    external get : elt t -> int -> elt Js.undefined = "" [@@bs.get_index]

    let opt_get f i = 
        Js.Undefined.toOption @@ get f i 
end

module Int_arr = Make ( struct type elt = int end)


let f v : int  * _ option = 
    Int_arr.unsafe_get v 0 , 
    Int_arr.opt_get v 1