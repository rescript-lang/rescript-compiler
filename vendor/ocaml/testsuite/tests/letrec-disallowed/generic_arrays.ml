(* This is not allowed because constructing the generic array 'x' involves
   inspecting 'y', which is bound in the same recursive group *)
let f z = let rec x = [| y; z |] and y = z in x;;
