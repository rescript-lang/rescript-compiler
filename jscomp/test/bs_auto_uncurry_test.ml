let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y =
  incr test_id ;
  suites :=
    (loc ^" id " ^ (string_of_int !test_id), (fun _ -> Mt.Eq(x,y))) :: !suites


external map :
     ('a -> 'b [@bs.uncurry]) -> 'b array =
     "map" [@@bs.send.pipe: 'a array]


[%%raw{|
function hi (cb){
    cb ();
    return 0;
}
|}]

external hi : (unit -> unit [@bs.uncurry]) -> unit = "hi" [@@bs.val]

let () =
    let xs = ref [] in
    hi (fun (() as x) -> xs := x ::!xs ) ;
    hi (fun (() as x) -> xs := x ::!xs ) ;
    eq __LOC__ !xs [();()]



let () =
    begin
    eq __LOC__
    ([|1;2;3|] |> map (fun x -> x + 1))
    ([|2;3;4|]);
    eq __LOC__
    ([|1;2;3|] |. Js.Array2.map (fun x -> x + 1))
    ([|2;3;4|]);

    eq __LOC__
    ([|1;2;3|] |. Js.Array2.reduce (+) 0)
    6 ;

    eq __LOC__
    ([|1;2;3|] |. Js.Array2.reducei (fun x y i -> x + y + i) 0)
    9;

    eq __LOC__
    ([| 1;2;3|] |. Js.Array2.some (fun x -> x <1))
    false ;

    eq __LOC__
    ([|1;2;3|] |. Js.Array2.every (fun x -> x > 0))
    true

    end




let () =
    Mt.from_pair_suites __MODULE__ !suites