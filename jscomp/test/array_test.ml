
open Mt

module type ARRAY = module type of Array 
module Make (Array : ARRAY) = struct 

let starts_with xs prefix p =   
  let module X = struct exception H end in
  let len1, len2 = Array.(length xs, length prefix) in 
  if len2 > len1 then false 
  else 
  try
    for i = 0 to len2 - 1 do 
      if not @@ p xs.(i) prefix.(i) then
        raise X.H
    done ;
    true
  with X.H -> false

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
      Eq (
        (Array.make 100 'a',
         Array.make_float 100),
        (Array.init 100 (fun _ -> 'a'),
        Array.init 100 (fun _ -> 0.)))
    );
  "sub", (fun _ -> 
    Eq ((Array.sub [|0;1;2;3;4|] 2 2), [|2;3|]));

  "blit", (fun _ -> 
    let u = [|100;0;0|] in
    let v = Array.init 3 (fun x -> x * 2 ) in
    let ()  = Array.blit v 1 u 1 2  in
    Eq (([|0;2;4|], [|100;2;4|]) , (v,u)));
  __LOC__, Array.(fun _ -> 
    let a0  = init 100 (fun i -> i * 1) in 
    blit a0 10 a0 5 20;
    Eq(true, 
    starts_with a0 
    [|0; 1; 2; 3; 4; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20; 21; 22; 23; 24;
  25; 26; 27; 28|] (=)
    )
  );    
  __LOC__, Array.(fun _ -> 
    let a0  = init 100 (fun i -> i * 1) in 
    blit a0 5 a0 10 20;
    Eq(true, 
    starts_with a0 
    [|0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16;
  17; 18; 19; 20|]
     (=)
    )
  );    
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



