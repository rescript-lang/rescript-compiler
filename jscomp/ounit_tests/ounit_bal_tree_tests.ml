let ((>::),
     (>:::)) = OUnit.((>::),(>:::))

let (=~) = OUnit.assert_equal

module Set_poly = Set_int
let suites = 
  __FILE__ >:::
  [
    __LOC__ >:: begin fun _ ->
      OUnit.assert_bool __LOC__
        (Set_poly.invariant 
           (Set_poly.of_array (Array.init 1000 (fun n -> n))))
    end;
    __LOC__ >:: begin fun _ ->
      OUnit.assert_bool __LOC__
        (Set_poly.invariant 
           (Set_poly.of_array (Array.init 1000 (fun n -> 1000-n))))
    end;
    __LOC__ >:: begin fun _ ->
      OUnit.assert_bool __LOC__
        (Set_poly.invariant 
           (Set_poly.of_array (Array.init 1000 (fun n -> Random.int 1000))))
    end;
    __LOC__ >:: begin fun _ ->
      OUnit.assert_bool __LOC__
        (Set_poly.invariant 
           (Set_poly.of_sorted_list (Array.to_list (Array.init 1000 (fun n -> n)))))
    end;
    __LOC__ >:: begin fun _ ->
      let arr = Array.init 1000 (fun n -> n) in
      let set = (Set_poly.of_sorted_array arr) in
      OUnit.assert_bool __LOC__
        (Set_poly.invariant set );
      OUnit.assert_equal 1000 (Set_poly.cardinal set)    
    end;
    __LOC__ >:: begin fun _ ->
      for i = 0 to 200 do 
        let arr = Array.init i (fun n -> n) in
        let set = (Set_poly.of_sorted_array arr) in
        OUnit.assert_bool __LOC__
          (Set_poly.invariant set );
        OUnit.assert_equal i (Set_poly.cardinal set)
      done    
    end;
    __LOC__ >:: begin fun _ ->
      let arr_size = 200 in
      let arr_sets = Array.make 200 Set_poly.empty in  
      for i = 0 to arr_size - 1 do
        let size = Random.int 1000 in  
        let arr = Array.init size (fun n -> n) in
        arr_sets.(i)<- (Set_poly.of_sorted_array arr)            
      done;
      let large = Array.fold_left Set_poly.union Set_poly.empty arr_sets in 
      OUnit.assert_bool __LOC__ (Set_poly.invariant large)
    end;

     __LOC__ >:: begin fun _ ->
      let arr_size = 1_00_000 in
      let v = ref Set_int.empty in 
      for i = 0 to arr_size - 1 do
        let size = Random.int 0x3FFFFFFF in  
         v := Set_int.add size !v                      
      done;       
      OUnit.assert_bool __LOC__ (Set_int.invariant !v)
    end;

  ]


type ident = { stamp : int ; name : string ; mutable flags : int}

module Ident_set = Set.Make(struct type t = ident 
    let compare = Pervasives.compare end)

let compare_ident x y = 
  let a =  compare (x.stamp : int) y.stamp in 
  if a <> 0 then a 
  else 
    let b = compare (x.name : string) y.name in 
    if b <> 0 then b 
    else compare (x.flags : int) y.flags     

let rec add x (tree : _ Set_gen.t0) : _ Set_gen.t0 =
  match tree with  
    | Empty -> Node(Empty, x, Empty, 1)
  | Node(l, v, r, _) as t ->
    let c = compare_ident x v in
    if c = 0 then t else
    if c < 0 then Set_gen.internal_bal (add x l) v r else Set_gen.internal_bal l v (add x r)

let rec mem x (tree : _ Set_gen.t0) = 
  match tree with 
   | Empty -> false
   | Node(l, v, r, _) ->
    let c = compare_ident x v in
    c = 0 || mem x (if c < 0 then l else r)

module Ident_set2 = Set.Make(struct type t = ident 
    let compare  = compare_ident            
  end)

let bench () = 
  let times = 1_000_000 in
  Ounit_tests_util.time "functor set" begin fun _ -> 
    let v = ref Ident_set.empty in  
    for i = 0 to  times do
      v := Ident_set.add   {stamp = i ; name = "name"; flags = -1 } !v 
    done;
    for i = 0 to times do
      ignore @@ Ident_set.mem   {stamp = i; name = "name" ; flags = -1} !v 
    done 
  end ;
  Ounit_tests_util.time "functor set (specialized)" begin fun _ -> 
    let v = ref Ident_set2.empty in  
    for i = 0 to  times do
      v := Ident_set2.add   {stamp = i ; name = "name"; flags = -1 } !v 
    done;
    for i = 0 to times do
      ignore @@ Ident_set2.mem   {stamp = i; name = "name" ; flags = -1} !v 
    done 
  end ;

  Ounit_tests_util.time "poly set" begin fun _ -> 
    let module Set_poly = Ident_set in 
    let v = ref Set_poly.empty in  
    for i = 0 to  times do
      v := Set_poly.add   {stamp = i ; name = "name"; flags = -1 } !v 
    done;
    for i = 0 to times do
      ignore @@ Set_poly.mem   {stamp = i; name = "name" ; flags = -1} !v 
    done;
  end;
  Ounit_tests_util.time "poly set (specialized)" begin fun _ -> 
    let v = ref Set_gen.empty in  
    for i = 0 to  times do
      v := add   {stamp = i ; name = "name"; flags = -1 } !v 
    done;
    for i = 0 to times do
      ignore @@ mem   {stamp = i; name = "name" ; flags = -1} !v 
    done 

  end ; 
