module Make( S : Map.OrderedType) = struct
  include Map.Make(S)
  let of_list (xs : ('a * 'b) list ) = 
    List.fold_left (fun acc (k,v) -> add k v acc) empty xs     
end
