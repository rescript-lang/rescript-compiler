module NotOption = struct

  type 'a t = None | Some of 'a
end

let a = NotOption.None (* undefined *)
let b = NotOption.Some(1) (* 1 *)
let c = let open NotOption in None 
let d = let open NotOption in Some(1) 


module TotallyNotOption = struct
  type 'a t = Some of ('a) | None
end

let e = TotallyNotOption.None (* undefined *)
let f = TotallyNotOption.Some(1) (* 1 *)
let g = let open TotallyNotOption in  None 
let h = let open TotallyNotOption in Some(1) 


module NotOption2 = struct
  type 'a t = None | Some of 'a | Bogus
end

let i = NotOption2.None 
let j = NotOption2.Some(1) 
let k = let open NotOption2 in None 
let l = let open NotOption2 in  Some(1) 