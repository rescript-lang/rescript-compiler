let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq f = Mt_global.collect_eq test_id suites f 

let test_strings = 
  Array.init 32 (fun i -> String.make i (Char.chr i))

let test_strings_hash_results = 
  [|0; 904391063; 889600889; 929588010; 596566298; 365199070; 448044845;
    311625091; 681445541; 634941451; 82108334; 17482990; 491949228; 696194769;
    711728152; 594966620; 820561748; 958901713; 102794744; 378848504;
    349314368; 114167579; 71240932; 110067399; 280623927; 323523937; 310683234;
    178511779; 585018975; 544388424; 1043872806; 831138595|]

let normalize x = x land 0x3FFFFFFF
let caml_hash x = Hashtbl.hash x |> normalize
let () = 
  eq __LOC__ 
    (test_strings |> Array.map caml_hash)
    test_strings_hash_results


let () =
  eq __LOC__ (Hashtbl.hash 0 |> normalize) 129913994

let () =
  eq __LOC__ (Hashtbl.hash "x" |> normalize) 780510073

let () =
  eq __LOC__ (Hashtbl.hash "xy" |> normalize) 194127723

let () =
  eq __LOC__ (caml_hash `A) 381663642;
  eq __LOC__ (caml_hash (`A 3)) 294279345;
  eq __LOC__ (caml_hash [`A 3; `B 2 ; `C 3 ]) 1017654909;
  eq __LOC__ (caml_hash [`A "3", `B "2" ; `C "3", `D "4"]) (81986873)  

let () =
  eq __LOC__ (caml_hash ([ `A (0,2,1), `B [| "x", "y"|]])) 100650590

let () =
  Mt.from_pair_suites __MODULE__ !suites
