let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = 
  incr test_id ; 
  suites := 
    (loc ^" id " ^ (string_of_int !test_id), (fun _ -> Mt.Eq(x,y))) :: !suites


external map :
     ('a -> 'b [@bs.uncurry]) -> 'b array =
     "" [@@bs.send.pipe: 'a array]

let () =
    begin 
    eq __LOC__ 
    ([|1;2;3|] |> map (fun x -> x + 1))
    ([|2;3;4|]);
    eq __LOC__
    ([|1;2;3|] |> Js_array.map (fun x -> x + 1))
    ([|2;3;4|]);

    eq __LOC__ 
    ([|1;2;3|] |> Js.Array.reduce (+) 0)
    6 ; 

    eq __LOC__ 
    ([|1;2;3|] |> Js.Array.reducei (fun x y i -> x + y + i) 0)
    9;

    eq __LOC__
    ([| 1;2;3|] |> Js.Array.some (fun x -> x <1))
    Js.false_ ;
    
    eq __LOC__ 
    ([|1;2;3|] |> Js.Array.every (fun x -> x > 0))
    Js.true_

    end




let () = 
    Mt.from_pair_suites __FILE__ !suites