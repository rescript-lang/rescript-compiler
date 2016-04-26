
let v = ref 0 

let reset, incr = 
  (fun _ ->  v:=0), (fun _ -> incr v)
