let ((>::),
     (>:::)) = OUnit.((>::),(>:::))

open Ext_json

let v = Int_vec.init 10 (fun i -> i);;
let (=~) x y = OUnit.assert_equal ~cmp:(Int_vec.equal  (fun (x: int) y -> x=y)) x y
let (=~~) x y 
  = 
  OUnit.assert_equal ~cmp:(Int_vec.equal  (fun (x: int) y -> x=y)) 
  x (Int_vec.of_array y) 

let suites = 
  __FILE__ 
  >:::
  [
    (** idea 
      [%loc "inplace filter" ] --> __LOC__ ^ "inplace filter" 
      or "inplace filter" [@bs.loc]
    *)
    "inplace_filter " ^ __LOC__ >:: begin fun _ -> 
      v =~~ [|0; 1; 2; 3; 4; 5; 6; 7; 8; 9|];
      
      ignore @@ Int_vec.push  32 v;
      let capacity = Int_vec.capacity v  in 
      v =~~ [|0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 32|];
      Int_vec.inplace_filter (fun x -> x mod 2 = 0) v ;
      v =~~ [|0; 2; 4; 6; 8; 32|];
      Int_vec.inplace_filter (fun x -> x mod 3 = 0) v ;
      v =~~ [|0;6|];
      Int_vec.inplace_filter (fun x -> x mod 3 <> 0) v ;
      v =~~ [||];
      OUnit.assert_equal (Int_vec.capacity v ) capacity ;
      Int_vec.compact v ; 
      OUnit.assert_equal (Int_vec.capacity v ) 0 
    end
    ;
    "inplace_filter_from " ^ __LOC__ >:: begin fun _ -> 
      let v = Int_vec.of_array (Array.init 10 (fun i -> i)) in 
      v =~~ [|0; 1; 2; 3; 4; 5; 6; 7; 8; 9|]; 
      Int_vec.push 96 v  ;      
      Int_vec.inplace_filter_from 2 (fun x -> x mod 2 = 0) v ;
      v =~~ [|0; 1; 2; 4; 6; 8; 96|];
      Int_vec.inplace_filter_from 2 (fun x -> x mod 3 = 0) v ;
      v =~~ [|0; 1; 6; 96|];
      Int_vec.inplace_filter (fun x -> x mod 3 <> 0) v ;
      v =~~ [|1|];      
      Int_vec.compact v ; 
      OUnit.assert_equal (Int_vec.capacity v ) 1
    end
    ;
    "map " ^ __LOC__ >:: begin fun _ -> 
      let v = Int_vec.of_array (Array.init 1000 (fun i -> i )) in 
      Int_vec.map succ v =~~ (Array.init 1000 succ) ;
      OUnit.assert_bool __LOC__ (Int_vec.exists (fun x -> x >= 999) v );
      OUnit.assert_bool __LOC__ (not (Int_vec.exists (fun x -> x > 1000) v ));
      OUnit.assert_equal (Int_vec.last v ) 999
    end ;  
    __LOC__ >:: begin fun _ -> 
      let count = 1000 in 
      let init_array = (Array.init count (fun i -> i)) in 
      let u = Int_vec.of_array  init_array in 
      let v = Int_vec.inplace_filter_with (fun x -> x mod 2 = 0) ~cb_no:Set_int.add Set_int.empty u  in
      let (even,odd) = init_array |> Array.to_list |> List.partition (fun x -> x mod 2 = 0) in 
      OUnit.assert_equal 
      (Set_int.elements v) odd ;
      u =~~ Array.of_list even 
    end ;
    "filter" ^ __LOC__ >:: begin fun _ -> 
      let v = Int_vec.of_array [|1;2;3;4;5;6|] in 
      v |> Int_vec.filter (fun x -> x mod 3 = 0) |> (fun x -> x =~~ [|3;6|]);
      v =~~ [|1;2;3;4;5;6|];
      Int_vec.pop v ; 
      v =~~ [|1;2;3;4;5|];
      let count = ref 0 in 
      let len = Int_vec.length v  in 
      while not (Int_vec.is_empty v ) do 
        Int_vec.pop v ;
        incr count
      done;
      OUnit.assert_equal len !count
    end
    ;
    __LOC__ >:: begin fun _ -> 
      let count = 100 in 
      let v = Int_vec.of_array (Array.init count (fun i -> i)) in 
      OUnit.assert_bool __LOC__ 
        (try Int_vec.delete v count; false with _ -> true );
      for i = count - 1 downto 10 do 
        Int_vec.delete v i ;
      done ;
      v =~~ [|0;1;2;3;4;5;6;7;8;9|] 
    end; 
    "sub" ^ __LOC__ >:: begin fun _ -> 
      let v = Int_vec.make 5 in 
      OUnit.assert_bool __LOC__
        (try ignore @@ Int_vec.sub v 0 2 ; false with Invalid_argument _  -> true);
      Int_vec.push 1 v;
      OUnit.assert_bool __LOC__
        (try ignore @@ Int_vec.sub v 0 2 ; false with Invalid_argument _  -> true);
      Int_vec.push 2 v ;  
      ( Int_vec.sub v 0 2 =~~ [|1;2|])
    end;
    "reserve" ^ __LOC__ >:: begin fun _ -> 
      let v = Int_vec.empty () in 
      Int_vec.reserve v  1000 ;
      for i = 0 to 900 do
        Int_vec.push i v 
      done ;
      OUnit.assert_equal (Int_vec.length v) 901 ;
      OUnit.assert_equal (Int_vec.capacity v) 1000
    end ; 
    "capacity"  ^ __LOC__ >:: begin fun _ -> 
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
    ;
    __LOC__ >:: begin fun _ ->
      let lst = [1;2;3;4] in 
      let v = Int_vec.of_list lst in 
      OUnit.assert_equal 
        (Int_vec.map_into_list (fun x -> x + 1) v)
        (Ext_list.map (fun x -> x + 1) lst)  
    end;
    __LOC__ >:: begin fun _ ->
      let v = Int_vec.make 4 in 
      Int_vec.push 1 v;
      Int_vec.push 2 v;
      Int_vec.reverse_in_place v;
      v =~~ [|2;1|]
    end
    ;
  ]
