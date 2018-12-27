
open Mt
module Int = struct 
  type t  = int
  let compare (x : int) (y : int) =  Pervasives.compare x y
end
module Int_map = Map.Make(Int)
module String_map = Map.Make(String)

let of_list kvs = 
  List.fold_left (fun acc (k,v) -> Int_map.add k v acc) Int_map.empty kvs

let int_map_suites = let open Mt in Int_map.[
  "add", (fun _ -> 
    let v = of_list [ 1,'1';2,'3';3,'4'] in
    Eq (cardinal v , 3)
         );
  "equal", (fun _ -> 
    let v = of_list [ 1,'1';2,'3';3,'4'] in
    let u = of_list [ 2,'3';3,'4'; 1,'1'] in
    Eq (compare Pervasives.compare u v , 0)
           );
  "equal2", (fun _ -> 
    let v = of_list [ 1,'1';2,'3';3,'4'] in
    let u = of_list [ 2,'3';3,'4'; 1,'1'] in
    Eq (true, equal (fun x y -> x = y) u v     )
    );



  "iteration", (fun _ -> 
    let m = ref String_map.empty in
    let count = 1_0000 in
    for i = 0 to count do 
      m := String_map.add (string_of_int i) (string_of_int i) !m
    done;
    let v = ref (-1) in
    for i = 0 to count  do 
      if (String_map.find (string_of_int i) !m ) != (string_of_int i) then 
        v:= i
    done; 
    Eq(!v, -1  )
               )
]
;; Mt.from_pair_suites __MODULE__ int_map_suites 
