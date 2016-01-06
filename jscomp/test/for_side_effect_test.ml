let tst () =
  for i = (print_endline"hi"; 0) to (print_endline "hello"; 3) do () done;;



let test2 () = 
  let v = ref 0  in
  for i = (v:=3; 0) to (v:=10;1) do 
    ()
  done;
  !v
open Mt 

let suites = 
  [ "for_order", (fun _ -> assert_equal 10 (test2 ()))]
;;

from_suites __FILE__ suites
(**
   {[
   var i = console.log("hi"),0;
   var length = console.log("hello"),3;

   for( ;i <= length;++i){
   }

   while(i ++ <= length){
   }
   
   ]}
 *)
