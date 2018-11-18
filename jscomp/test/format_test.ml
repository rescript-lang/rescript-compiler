let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = 
  incr test_id ; 
  suites := 
    (loc ^" id " ^ (string_of_int !test_id), (fun _ -> Mt.Eq(x,y))) :: !suites

let eq3 loc a b c = 
  eq loc a b ; 
  eq loc b c ; 
  eq loc a c 

let u () = "xx %s" ^^ "yy"

module M = struct 
  external infinity : float = "POSITIVE_INFINITY" 
  [@@bs.val]  [@@bs.scope "Number"]
  external neg_infinity : float = "NEGATIVE_INFINITY"
  [@@bs.val]  [@@bs.scope "Number"]
  external nan : float = "NaN"
  [@@bs.val]  [@@bs.scope "Number"]
  external max_float : float = "MAX_VALUE"
  [@@bs.val]  [@@bs.scope "Number"]
end 
let () = 
  eq __LOC__ (Format.asprintf (u ()) "x") ("xx x" ^ "yy")
  

#if OCAML_VERSION =~ ">4.03.0" then
let () = 
  eq __LOC__ (0x3.fp+1) (7.875);


  eq __LOC__ (-0x3.fp+1) (-7.875);  
  (* in standard, it is still infinity, but 
    ideally should give an warning
  *)
  eq3 __LOC__ 0x1p2047 M.infinity infinity;
  eq3 __LOC__ (-0x1p2047) M.neg_infinity neg_infinity;  
  eq3 __LOC__ max_float 0x1.ffff_ffff_ffff_fp+1023 M.max_float;  
  eq __LOC__ (classify_float 0x1.2p2047) FP_infinite;
  eq __LOC__ (classify_float 0x1.1p2047) FP_infinite;
  
  
  eq __LOC__ min_float 0x1p-1022;
  eq __LOC__ epsilon_float 0x1p-52;
  eq __LOC__ 0x0.0000_0000_0000_1p-1022 5e-324;
  eq __LOC__  
    (0x1.0000_0000_0000_1 -. 1.) epsilon_float;
  eq __LOC__
    (0x1p-1023 /. 0x1p-1022) 0x1p-1;
  eq __LOC__ (classify_float 0x1p-1023) FP_subnormal;
  eq __LOC__  
    0x1p-1023 0x0.8p-1022;
  eq __LOC__  
    0x0.ffff_ffff_ffff_ffff_ffp-1022
    0x1p-1022;

  eq __LOC__  
    ((1. +. 0xffp0 /. 0x100p0 ) *. 8.)
    (0x1.ffp3);
  eq __LOC__  
    ((1. +. 0xfffp0 /. 0x1000p0) *. 8.)
    0x1.fffp3;
  eq __LOC__  
    ((1. +. 0xffffp0 /. 0x10000p0) *. 8.)
    0x1.ffffp3    
  ;;
#end  
(*TODO: add scanf example *)

let f loc ls  = 
  List.iter  (fun (a,b) -> 
  eq loc (float_of_string a) b ) ls

#if OCAML_VERSION=~ ">4.03.0" then
let () = 
    f __LOC__ [
      "0x3.fp+1",  0x3.fp+1 ;
      " 0x3.fp2", 0x3.fp2;
      " 0x4.fp2", 0x4.fp2
      ];
    
#end     
;;

#if 
  (* OCAML_VERSION =~ ">4.03.0" *) 0
then




let () = eq __LOC__ (Printf.sprintf "%h" 0x3.fp+1) "0x1.f8p+2"
let () = eq __LOC__ (Printf.sprintf "%H" 0x3.fp+1) "0x1.F8P+2"

let () = eq __LOC__ (Printf.sprintf "%h" 0.3) "0x1.3333333333333p-2"

#end
let () = Mt.from_pair_suites __FILE__ !suites
