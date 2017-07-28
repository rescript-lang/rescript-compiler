module M = struct
 type t = string

 let x = 0
 let x = 1

 module Set = Set.Make(String)
end

include M
