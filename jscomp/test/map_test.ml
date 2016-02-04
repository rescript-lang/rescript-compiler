
open Mt
module Int = struct 
  type t  = int
  let compare (x : int) (y : int) =  Pervasives.compare x y
end
module Int_map = Map.Make(Int)
module String_map = Map.Make(String)

let of_list kvs = 
  List.fold_left (fun acc (k,v) -> Int_map.add k v acc) Int_map.empty kvs

let int_map_suites = Int_map.[
  "add", (fun _ -> 
    let v = of_list [ 1,'1';2,'3';3,'4'] in
    assert (cardinal v = 3)
         );
  "equal", (fun _ -> 
    let v = of_list [ 1,'1';2,'3';3,'4'] in
    let u = of_list [ 2,'3';3,'4'; 1,'1'] in
    assert (compare Pervasives.compare u v = 0)
           );
  "equal2", (fun _ -> 
    let v = of_list [ 1,'1';2,'3';3,'4'] in
    let u = of_list [ 2,'3';3,'4'; 1,'1'] in
    assert (equal (fun x y -> x = y) u v     )
    );
  "test_inline_map", Test_inline_map.assertions;
  "test_inline_map2", Test_inline_map2.assertions1; 
  "test_inline_map2_1", Test_inline_map2.assertions2; 

  "test_map_find", Test_map_find.assert_test;
  "iteration", (fun _ -> 
    let m = ref String_map.empty in
    let count = 10000 in
    for i = 0 to count do 
      m := String_map.add (string_of_int i) (string_of_int i) !m
    done;

    for i = 0 to count  do 
      Mt.assert_equal (String_map.find (string_of_int i) !m ) (string_of_int i)
    done
               )
]
;; from_suites "map_test" int_map_suites 
