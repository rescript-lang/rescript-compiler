
let simpleEq = (fun [@bs] a b -> a = b)
  
let option_suites = Mt.[
    "option_isSome_Some" , (fun _ -> 
        Eq (true , Js.Option.isSome (Some 1))
      );
    "option_isSome_None" , (fun _ -> 
        Eq (false , Js.Option.isSome None)
      );
    "option_isNone_Some" , (fun _ -> 
        Eq (false , Js.Option.isNone (Some 1))
      );
    "option_isNone_None" , (fun _ -> 
        Eq (true , Js.Option.isNone None)
      );
    "option_isSomeValue_Eq", (fun _ -> 
        Eq (true , Js.Option.isSomeValue simpleEq 2 (Some 2))
      );
    "option_isSomeValue_Diff", (fun _ -> 
        Eq (false , Js.Option.isSomeValue simpleEq 1 (Some 2))
      );
    "option_isSomeValue_DiffNone", (fun _ -> 
        Eq (false , Js.Option.isSomeValue simpleEq 1 None)
      );
    "option_getExn_Some" , (fun _ -> 
        Eq (2 , Js.Option.getExn (Some 2))
      );
    "option_equal_Eq", (fun _ -> 
        Eq (true , Js.Option.equal simpleEq (Some 2) (Some 2))
      );
    "option_equal_Diff", (fun _ -> 
        Eq (false , Js.Option.equal simpleEq (Some 1) (Some 2))
      );
    "option_equal_DiffNone", (fun _ -> 
        Eq (false , Js.Option.equal simpleEq (Some 1) None)
      );
    "option_andThen_SomeSome", (fun _ -> 
        Eq (true , 
            Js.Option.isSomeValue simpleEq 3 (Js.Option.andThen (fun [@bs] a -> Some (a + 1)) (Some 2)))
      );
    "option_andThen_SomeNone", (fun _ -> 
        Eq (false , 
            Js.Option.isSomeValue simpleEq 3 (Js.Option.andThen (fun [@bs] _ -> None) (Some 2)))
      );
    "option_map_Some", (fun _ -> 
        Eq (true , 
            Js.Option.isSomeValue simpleEq 3 (Js.Option.map (fun [@bs] a -> a + 1) (Some 2)))
      );
    "option_map_None", (fun _ -> 
        Eq (None , Js.Option.map (fun [@bs] a -> a + 1) None)
      );
    "option_default_Some", (fun _ -> 
        Eq (2 , Js.Option.getWithDefault 3 (Some 2))
      );
    "option_default_None", (fun _ -> 
        Eq (3 , Js.Option.getWithDefault 3 None)
      );
    "option_filter_Pass", (fun _ -> 
        Eq (true , 
            Js.Option.isSomeValue simpleEq 2 (Js.Option.filter (fun [@bs] a -> (a mod 2) = 0) (Some 2)))
      );
    "option_filter_Reject", (fun _ -> 
        Eq (None , Js.Option.filter (fun [@bs] a -> (a mod 3) = 0) (Some 2))
      );
    "option_filter_None", (fun _ -> 
        Eq (None , Js.Option.filter (fun [@bs] a -> (a mod 3) = 0) None)
      );
    "option_firstSome_First", (fun _ -> 
        Eq (true , 
            Js.Option.isSomeValue simpleEq 3 (Js.Option.firstSome (Some 3) (Some 2)))
      );
    "option_firstSome_First", (fun _ -> 
        Eq (true , 
            Js.Option.isSomeValue simpleEq 2 (Js.Option.firstSome None (Some 2)))
      );
    "option_firstSome_None", (fun _ -> 
        Eq (None , Js.Option.firstSome None None)
      );
    
  ]

let () = 
  begin
    Mt.from_pair_suites __FILE__ option_suites
  end
