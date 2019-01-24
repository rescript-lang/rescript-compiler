let f () =
   let module S = String in
   let module N = Map.Make(S) in
   N.add "sum" 41 N.empty;;
