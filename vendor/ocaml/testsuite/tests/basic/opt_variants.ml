let () =
  assert(Sys.getenv_opt "FOOBAR_UNLIKELY_TO_EXIST_42" = None);

  assert(int_of_string_opt "foo" = None);
  assert(int_of_string_opt "42" = Some 42);
  assert(int_of_string_opt (String.make 100 '9') = None);

  assert(Nativeint.of_string_opt "foo" = None);
  assert(Nativeint.of_string_opt "42" = Some 42n);
  assert(Nativeint.of_string_opt (String.make 100 '9') = None);

  assert(Int32.of_string_opt "foo" = None);
  assert(Int32.of_string_opt "42" = Some 42l);
  assert(Int32.of_string_opt (String.make 100 '9') = None);

  assert(Int64.of_string_opt "foo" = None);
  assert(Int64.of_string_opt "42" = Some 42L);
  assert(Int64.of_string_opt (String.make 100 '9') = None);

  assert(bool_of_string_opt "" = None);
  assert(bool_of_string_opt "true" = Some true);
  assert(bool_of_string_opt "false" = Some false);

  assert(float_of_string_opt "foo" = None);
  assert(float_of_string_opt "42." = Some 42.);
  assert(float_of_string_opt (String.make 1000 '9') = Some infinity);

  assert(List.nth_opt [] 0 = None);
  assert(List.nth_opt [42] 0 = Some 42);
  assert(List.nth_opt [42] 1 = None);

  assert(List.find_opt (fun _ -> true) [] = None);
  assert(List.find_opt (fun x -> x > 10) [4; 42] = Some 42);

  assert(List.assoc_opt 42 [] = None);
  assert(List.assoc_opt 42 [41, false; 42, true] = Some true);

  assert(List.assq_opt 42 [] = None);
  assert(List.assq_opt 42 [41, false; 42, true] = Some true);

  let h = Hashtbl.create 5 in
  assert(Hashtbl.find_opt h 42 = None);
  Hashtbl.add h 42 ();
  assert(Hashtbl.find_opt h 42 = Some ());


  let module IntSet = Set.Make(struct
      type t = int
      let compare = compare
    end)
  in
  let set = IntSet.of_list [42; 43] in
  assert(IntSet.min_elt_opt IntSet.empty = None);
  assert(IntSet.min_elt_opt set = Some 42);

  assert(IntSet.max_elt_opt IntSet.empty = None);
  assert(IntSet.max_elt_opt set = Some 43);

  assert(IntSet.choose_opt IntSet.empty = None);
  assert(IntSet.choose_opt set <> None);

  assert(IntSet.find_opt 42 IntSet.empty = None);
  assert(IntSet.find_opt 42 set = Some 42);
  assert(IntSet.find_opt 0 set = None);


  let module IntMap = Map.Make(struct
      type t = int
      let compare = compare
    end)
  in
  let map = IntMap.add 42 "42" (IntMap.add 43 "43" IntMap.empty) in
  assert(IntMap.min_binding_opt IntMap.empty = None);
  assert(IntMap.min_binding_opt map = Some (42, "42"));

  assert(IntMap.max_binding_opt IntMap.empty = None);
  assert(IntMap.max_binding_opt map = Some (43, "43"));

  assert(IntMap.choose_opt IntMap.empty = None);
  assert(IntMap.choose_opt map <> None);

  assert(IntMap.find_opt 42 IntMap.empty = None);
  assert(IntMap.find_opt 42 map = Some "42");
  assert(IntMap.find_opt 0 map = None);


  let s = "Hello world !" in
  assert(String.index_opt s 'x'  = None);
  assert(String.index_opt s ' '  = Some 5);

  assert(String.rindex_opt s 'x'  = None);
  assert(String.rindex_opt s ' '  = Some 11);

  assert(String.index_from_opt s 0 'x'  = None);
  assert(String.index_from_opt s 6 ' '  = Some 11);

  assert(String.rindex_from_opt s 0 'x'  = None);
  assert(String.rindex_from_opt s 6 ' '  = Some 5);


  let module W = Weak.Make(struct
      type t = int ref
      let equal = (=)
      let hash = Hashtbl.hash
    end)
  in
  let w = W.create 10 in
  let x = Random.int 42 in
  let r = ref x in
  assert (W.find_opt w r = None);
  W.add w r;
  assert (W.find_opt w r = Some r);

  ()
