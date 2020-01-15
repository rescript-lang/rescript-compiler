
let f value =
  match (Js.Nullable.isNullable(value)) with
  | false -> Some(Obj.magic(value): string)
  | true -> None
                            

let fxx v = 
  match v ()with 
  | 1 -> 'a'
  | 2 -> 'b'
  | 3 -> 'c'
  | _ -> 'd'    


let fxxx2 v = 
  match v () with 
  | false -> 1 
  | true -> 2     

let fxxx3 v =  
  if v () then 2 else 1  