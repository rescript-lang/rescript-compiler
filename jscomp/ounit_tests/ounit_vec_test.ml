let ((>::),
    (>:::)) = OUnit.((>::),(>:::))

open Bsb_json

let v = Int_vec.init 10 (fun i -> i);;
let (=~) x y = OUnit.assert_equal ~cmp:(Int_vec.equal  (fun (x: int) y -> x=y)) x y
let (=~~) x y 
  = 
  OUnit.assert_equal ~cmp:(Int_vec.equal  (fun (x: int) y -> x=y)) x (Int_vec.of_array y) 

let suites = 
  __FILE__ 
  >:::
  [
    "inplace_filter" >:: begin fun _ -> 
      v =~~ [|0; 1; 2; 3; 4; 5; 6; 7; 8; 9|];
      ignore @@ Int_vec.push  32 v;
      v =~~ [|0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 32|];
      Int_vec.inplace_filter (fun x -> x mod 2 = 0) v ;
      v =~~ [|0; 2; 4; 6; 8; 32|];
      Int_vec.inplace_filter (fun x -> x mod 3 = 0) v ;
      v =~~ [|0;6|];
      Int_vec.inplace_filter (fun x -> x mod 3 <> 0) v ;
      v =~~ [||]
    end
    ;
    "filter" >:: begin fun _ -> 
      let v = Int_vec.of_array [|1;2;3;4;5;6|] in 
      v |> Int_vec.filter (fun x -> x mod 3 = 0) |> (fun x -> x =~~ [|3;6|]);
      v =~~ [|1;2;3;4;5;6|];
      Int_vec.pop v ; 
      v =~~ [|1;2;3;4;5|]
    end
    ;

    "capacity" >:: begin fun _ -> 
      let v = Int_vec.of_array [|3|] in 
      Int_vec.reserve v 10 ;
      v =~~ [|3 |];
      Int_vec.push 1 v ;
      Int_vec.push 2 v ;
      Int_vec.push 5 v ;
      v=~~ [|3;1;2;5|];
      OUnit.assert_equal (Int_vec.capacity v  ) 10 ;
      for i = 0 to 5 do
        Int_vec.push i  v
      done;
      v=~~ [|3;1;2;5;0;1;2;3;4;5|];
      Int_vec.push   100 v;
      v=~~[|3;1;2;5;0;1;2;3;4;5;100|];
      OUnit.assert_equal (Int_vec.capacity v ) 20
    end
    ;
    __LOC__  >:: begin fun _ -> 
      let empty = Int_vec.empty () in 
      Int_vec.push   3 empty;
      empty =~~ [|3|];

    end
  ]
