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
    ;
    __LOC__ >:: begin fun _ ->
      let v = Ordered_hash_set.create 3 in 
      for i =  0 to 10 do
        Ordered_hash_set.add v (string_of_int i) 
      done; 
      for i = 100 downto 2 do
        Ordered_hash_set.add v (string_of_int i)
      done;
      OUnit.assert_equal (Ordered_hash_set.to_sorted_array v )
        [|"0"; "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"; "10"; "100"; "99"; "98";
          "97"; "96"; "95"; "94"; "93"; "92"; "91"; "90"; "89"; "88"; "87"; "86"; "85";
          "84"; "83"; "82"; "81"; "80"; "79"; "78"; "77"; "76"; "75"; "74"; "73"; "72";
          "71"; "70"; "69"; "68"; "67"; "66"; "65"; "64"; "63"; "62"; "61"; "60"; "59";
          "58"; "57"; "56"; "55"; "54"; "53"; "52"; "51"; "50"; "49"; "48"; "47"; "46";
          "45"; "44"; "43"; "42"; "41"; "40"; "39"; "38"; "37"; "36"; "35"; "34"; "33";
          "32"; "31"; "30"; "29"; "28"; "27"; "26"; "25"; "24"; "23"; "22"; "21"; "20";
          "19"; "18"; "17"; "16"; "15"; "14"; "13"; "12"; "11"|]
    end;
  ]
