let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = 
  incr test_id ; 
  suites := 
    (loc ^" id " ^ (string_of_int !test_id), (fun _ -> Mt.Eq(x,y))) :: !suites



let () = 
    eq __LOC__
    (Js.List.flatten [[1;2];[3];[];[1;2;3]])
    [1;2;3;1;2;3];
    eq __LOC__
    (Js.List.filterMap (fun [@bs] x -> if x mod 2 = 0 then Some x else None) [1;2;3;4;5;6;7])
    [2;4;6];
    eq __LOC__
    (Js.List.filterMap (fun [@bs] x -> if x mod 2 = 0 then Some x else None) [1;2;3;4;5;6])
    [2;4;6];
    eq __LOC__ 
    (Js.List.countBy (fun[@bs] x -> x mod 2 = 0) [1;2;3;4;5;6]) 3;
    let v = Js.List.init 10_0000 (fun [@bs] i -> i) in 
    eq __LOC__ 
     (Js.List.countBy (fun[@bs] x -> x mod 2 = 0) v) 50_000;
    let vv = Js.List.foldRight 
        (fun[@bs] x y -> Js.List.cons x y)  v [] in 
    eq __LOC__ true 
    (Js.List.equal (fun[@bs] x y -> x = y ) v vv);
    
    let vvv = Js.List.filter (fun [@bs] x -> x mod 10 = 0) vv in 
    eq __LOC__ (Js.List.length vvv) 10000;
    eq __LOC__ true (Js.List.equal (fun [@bs] x y -> x = y) vvv (Js.List. init 10_000 (fun [@bs] x  -> x * 10)  ))


;; Mt.from_pair_suites __FILE__ !suites