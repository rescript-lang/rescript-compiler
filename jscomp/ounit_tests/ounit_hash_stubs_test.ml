let ((>::),
    (>:::)) = OUnit.((>::),(>:::))

let (=~) = OUnit.assert_equal

let count = 2_000_000

let bench () = 
  Ounit_tests_util.time "int hash set" begin fun _ -> 
    let v = Int_hash_set.create 2_000_000 in 
    for i = 0 to  count do 
      Int_hash_set.add  v i
    done ;
    for i = 0 to 3 do 
      for i = 0 to count do 
        assert (Int_hash_set.mem v i)
      done
    done
  end;
  Ounit_tests_util.time "int hash set" begin fun _ -> 
    let v = Hash_set.create 2_000_000 in 
    for i = 0 to  count do 
      Hash_set.add  v i
    done ;
    for i = 0 to 3 do 
      for i = 0 to count do 
        assert (Hash_set.mem v i)
     done
    done
  end

let suites = 
    __FILE__
    >:::
    [
      __LOC__ >:: begin fun _ -> 
        Bs_hash_stubs.hash_int 0 =~ Hashtbl.hash 0
      end;
      __LOC__ >:: begin fun _ -> 
        Bs_hash_stubs.hash_int max_int =~ Hashtbl.hash max_int
      end;
      __LOC__ >:: begin fun _ -> 
        Bs_hash_stubs.hash_int max_int =~ Hashtbl.hash max_int
      end

    ]
