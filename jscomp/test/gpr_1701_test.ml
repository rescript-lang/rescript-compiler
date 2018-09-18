

exception Foo 

let rec test n =
    if n = 0 then raise Foo
    else 
        try 
            test (n - 1)
        with Foo ->()


let () = test 100        



let read_lines inc =
   let rec loop acc =
     match (try Some (input_line inc)
            with End_of_file -> None)
     with
     | Some l -> loop (l :: acc)
     | None -> List.rev acc
   in
   loop []    

let read_lines2 inc =
   let rec loop acc =
     match input_line inc  with
     | l -> loop (l :: acc)
     | exception End_of_file -> List.rev acc
   in
   loop []   

let read_lines3 inc =
   let rec loop acc =
     try
       let l = input_line inc in
       loop (l :: acc)
     with End_of_file -> List.rev acc
   in
   loop []   


 let rec fff f x = 
   try fff f x with _ ->  x + 1