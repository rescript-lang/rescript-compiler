
open Mt

module type ARRAY = module type of Array 
module Make (Array : ARRAY) = struct 

let is_sorted x = 
  let len = Array.length x in
  let rec aux i  = 
    if i >= len - 1 then true
    else 
      if x.(i) < x.(i+1) then
        aux (i + 1)
      else 
        false in
  aux 0 
          
let array_suites = Mt.[
  "init", (fun _ -> 
    Eq (Array.init 5 (fun x -> x ),  [|0;1;2;3;4|]));

  "toList",(fun _ ->
    let  aux (xs : (int array * int list ) list)  = 
      List.fold_left (fun acc (x,  y ) -> (Array.to_list x, y)::acc  )
        [] xs 
    in
    let a,b = List.split @@ aux [ [||], [] ]    in
    Eq (a,b)

           );
  "concat", (fun _ -> 
     Eq([|0;1;2;3;4;5|] ,
            Array.concat [[|0;1;2|]; [|3;4|]; [||]; [|5|]]
            ));
  "make", (fun _ -> 
    Eq ((Array.make 100 'a'),
      (Array.init 100 (fun _ -> 'a'))
          ));
  "sub", (fun _ -> 
    Eq ((Array.sub [|0;1;2;3;4|] 2 2), [|2;3|]));

  "blit", (fun _ -> 
    let u = [|100;0;0|] in
    let v = Array.init 3 (fun x -> x * 2 ) in
    let ()  = Array.blit v 1 u 1 2  in
    Eq (([|0;2;4|], [|100;2;4|]) , (v,u)));

  "make", (fun _ -> 
    Eq (Array.make 2 1,  [|1;1|]));
  "sort", (fun _ ->
    let u  = [|3;0;1|] in
    Array.sort (fun (x:int)  y -> Pervasives.compare x y) u;
    Eq([|0;1;3|]= u, true) (* seems [assert.deepEqual] does not do the right job..*)
          );
  "sort_large", (fun _ -> 
    (** random test is hard to reproduce *)
    let v = Array.init 4 (fun i -> i mod 17)  in
    Array.sort (fun (x:int)  y -> compare x y) v ; 
    Eq(true, is_sorted v ))
]
end

;; from_pair_suites __FILE__
      (let module Array_test = Make(Array)in 
      Array_test.array_suites)



