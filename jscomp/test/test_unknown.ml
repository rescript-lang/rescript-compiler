

type t = unknown = 
 | Unknown : _ -> t [@@unboxed]


let some (x : unknown) = Some x 

let some2 x = Some (Unknown x) 

let h = [|Unknown 3 ; Unknown 2 ; Unknown (Some (Unknown 2))|]

type t0 =  Any : _ -> t0 [@@unboxed]

type t1 = t0 =   Any : _ -> t1 [@@unboxed]