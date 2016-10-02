

let vv = 
#if BS #then
    3
#else 
    1
#end


let v = ref 1 

let a = 
#if BS #then
 let () = incr v  in
#end !v 

let version_gt_3 =
#if BS_VERSION  (* comment *) =~ ">1" #then
    true
#else
    false
#end

let version =
#if BS_VERSION =~ "~2" #then
   2
#end
#if BS_VERSION =~ "~1" #then
   1
#end
#if BS_VERSION =~ "~0" #then
   0
#else
  -1
#end

let ocaml_veriosn = 
#if OCAML_VERSION =~ "~4.02.0" #then
   "4.02.3"
#else "unknown"
#end

(**
#if OCAML_VERSION =~ "4.02.3" #then

#elif OCAML_VERSION =~ "4.03" #then
   gsho
#end
*)
(* #if OCAML_VERSION =~ ">4.02" #then *)
(* #else *)
(* #end *)
let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = 
  incr test_id ; 
  suites := 
    (loc ^" id " ^ (string_of_int !test_id), (fun _ -> Mt.Eq(x,y))) :: !suites


let () = 
  eq __LOC__ vv 3  ;
  eq __LOC__ !v 2


;; Mt.from_pair_suites __FILE__ !suites
