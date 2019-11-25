type student = {
  name: string;
  age: int;
}
let _ =
  match (Some 1) with
  | Some 1 ->
    (match [%raw "1"] with
     | 1 -> {name = "hi"; age = 1}
     | 2 -> {
         name = "bye";
         age = Js.Math.floor 1.
       }
     | _ -> assert false
    ) 
   | _ -> assert false    