let ((>::),
    (>:::)) = OUnit.((>::),(>:::))

open Bsb_json
module Int_array = Resize_array.Make(struct type t = int let null = 0 end);;
let v = Int_array.init 10 (fun i -> i);;
let (=~) x y = OUnit.assert_equal ~cmp:(Int_array.equal  (fun (x: int) y -> x=y)) x y
let (=~~) x y 
  = 
  OUnit.assert_equal ~cmp:(Int_array.equal  (fun (x: int) y -> x=y)) x (Int_array.of_array y) 

let suites = 
  __FILE__ 
  >:::
  [
    "inplace_filter" >:: begin fun _ -> 
      v =~~ [|0; 1; 2; 3; 4; 5; 6; 7; 8; 9|];
      ignore @@ Int_array.push v 32;
      v =~~ [|0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 32|];
      Int_array.inplace_filter (fun x -> x mod 2 = 0) v ;
      v =~~ [|0; 2; 4; 6; 8; 32|];
      Int_array.inplace_filter (fun x -> x mod 3 = 0) v ;
      v =~~ [|0;6|];
      Int_array.inplace_filter (fun x -> x mod 3 <> 0) v ;
      v =~~ [||]
    end
    ;
    "filter" >:: begin fun _ -> 
      let v = Int_array.of_array [|1;2;3;4;5;6|] in 
      v |> Int_array.filter (fun x -> x mod 3 = 0) |> (fun x -> x =~~ [|3;6|]);
      v =~~ [|1;2;3;4;5;6|]
    end
    ;

    "capacity" >:: begin fun _ -> 
      let v = Int_array.of_array [|3|] in 
      Int_array.reserve v 10 ;
      v =~~ [|3 |];
      Int_array.push v 1;
      Int_array.push v 2;
      Int_array.push v 5;
      v=~~ [|3;1;2;5|];
      OUnit.assert_equal (Int_array.capacity v  ) 10 ;
      for i = 0 to 5 do
        Int_array.push v i
      done;
      v=~~ [|3;1;2;5;0;1;2;3;4;5|];
      Int_array.push v 100;
      v=~~[|3;1;2;5;0;1;2;3;4;5;100|];
      OUnit.assert_equal (Int_array.capacity v ) 20
    end

  ]
