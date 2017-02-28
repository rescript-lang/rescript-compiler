let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = 
  incr test_id ; 
  suites := 
    (loc ^" id " ^ (string_of_int !test_id), (fun _ -> Mt.Eq(x,y))) :: !suites

let convert (s : string) : int list  = 
    Js_array.fromMap 
        (Js_string.castToArrayLike s) 
        (fun x -> 
        match Js_undefined.to_opt @@ Js_string.codePointAt 0 x with 
        | None -> assert false 
        | Some x -> x ) |> Array.to_list  

let () = 
    begin 
    eq __LOC__
     (convert {j|汉字是世界上最美丽的character|j} )
 [27721;
  23383;
  26159;
  19990;
  30028;
  19978;
  26368;
  32654;
  20029;
  30340;
  99;
  104;
  97;
  114;
  97;
  99;
  116;
  101;
  114 ];
  eq __LOC__ (convert {j|\x3f\x3f|j})
  [63;63];
  eq __LOC__ (convert {j|??|j})
  [63;63];
  eq __LOC__ (convert {j|\u003f\x3f|j})
  [63;63];
  eq __LOC__ (convert {j|\u{01003f}\x3f|j})
  [65599;63];
  (* It is amazing Array.from(string) 
    is unicode safe *)
  eq __LOC__ (convert {j|\u{01003f}\u{01003f}|j})
  [65599;65599];
  (* "\u{11003f}\x3f 
    undefined unicode code piont
  *)
  eq __LOC__ (String.length {j|\u{01003f}|j} 2);

  eq __LOC__ (convert {j| \b\t\n\v\f\r|j})
  [ 32; 8; 9; 10; 11; 12; 13; ];

  (* we don't need escape string double quote {|"|}and single quote{|'|} 
    however when we print it, we need escape them 
    there is no need for line continuation,
    
  *)
  (* eq __LOC__ (convert {j| \b\t\n\v\f\r"'\\|j})
  [ 32; 8; 9; 10; 11; 12; 13; 34; 39; 92 ] *)
    end
let () = Mt.from_pair_suites __FILE__ !suites 