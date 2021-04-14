let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = 
  incr test_id ; 
  suites := 
    (loc ^" id " ^ (string_of_int !test_id), (fun _ -> Mt.Eq(x,y))) :: !suites



exception Hi 
exception Hello
exception A of int 
exception AAA = Exception_def.A 

;; Printexc.register_printer (function 
    | Hi  -> Some "hey"
    | A s -> Some ({j|A($(s))|j} )
    | _ -> None
)



let () = 
    eq __LOC__ "hey" (Printexc.to_string Hi);
    eq __LOC__ "A(1)" (Printexc.to_string (A 1));
    eq __LOC__  (Js.String2.startsWith (Printexc.to_string Hello) "Exception_repr_test.Hello") true;
    eq __LOC__ "A" (Printexc.to_string @@ AAA 3)

;; Mt.from_pair_suites __MODULE__ !suites


