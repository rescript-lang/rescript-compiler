let ((>::),
    (>:::)) = OUnit.((>::),(>:::))

let (=~) = OUnit.assert_equal

type id = { name : string ; stamp : int }

module Id_hash_set = Hash_set.Make(struct 
    type t = id 
    let equal x y = x.stamp = y.stamp && x.name = y.name 
    let hash x = Hashtbl.hash x.stamp
  end
)

let suites = 
    __FILE__
    >:::
    [
        __LOC__ >:: begin fun _ ->
            let v = Hash_set.create 31 in
            for i = 0 to 1000 do
                Hash_set.add v i  
            done  ;
            OUnit.assert_equal (Hash_set.length v) 1001
        end ;
        __LOC__ >:: begin fun _ ->
            let v = Hash_set.create 31 in
            for i = 0 to 1_0_000 do
                Hash_set.add v 0
            done  ;
            OUnit.assert_equal (Hash_set.length v) 1
        end ;
        __LOC__ >:: begin fun _ -> 
          let v = Hash_set.create 30 in 
          for i = 0 to 2_000 do 
            Hash_set.add v {name = "x" ; stamp = i}
          done ;
          for i = 0 to 2_000 do 
            Hash_set.add v {name = "x" ; stamp = i}
          done  ; 
          for i = 0 to 2_000 do 
            assert (Hash_set.mem v {name = "x"; stamp = i})
          done;  
          OUnit.assert_equal (Hash_set.length v)  2_001;
          for i =  1990 to 3_000 do 
            Hash_set.remove v {name = "x"; stamp = i}
          done ;
          OUnit.assert_equal (Hash_set.length v) 1990;
          OUnit.assert_equal (Hash_set.stats v)
            {Hash_set.num_bindings = 1990; num_buckets = 1024; max_bucket_length = 7;
             bucket_histogram = [|139; 303; 264; 178; 93; 32; 12; 3|]}
        end ;
        __LOC__ >:: begin fun _ -> 
          let module Hash_set = Id_hash_set in 
          let v = Hash_set.create 30 in 
          for i = 0 to 2_000 do 
            Hash_set.add v {name = "x" ; stamp = i}
          done ;
          for i = 0 to 2_000 do 
            Hash_set.add v {name = "x" ; stamp = i}
          done  ; 
          for i = 0 to 2_000 do 
            assert (Hash_set.mem v {name = "x"; stamp = i})
          done;  
          OUnit.assert_equal (Hash_set.length v)  2_001;
          for i =  1990 to 3_000 do 
            Hash_set.remove v {name = "x"; stamp = i}
          done ;
          OUnit.assert_equal (Hash_set.length v) 1990;
          OUnit.assert_equal (Hash_set.stats v)
            {num_bindings = 1990; num_buckets = 1024; max_bucket_length = 8;
             bucket_histogram = [|148; 275; 285; 182; 95; 21; 14; 2; 2|]}
            
        end 

    ]
