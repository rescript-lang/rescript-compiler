let v =
#if BS then  
   true
#else    
  false
#end  

let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = Mt.eq_suites ~test_id ~suites loc x y 
let b loc x = Mt.bool_suites ~test_id ~suites loc x;;


#if 1 then 
b __LOC__  true;;
#end

#if 0 then 
b __LOC__ false ;;
#end

#if 1 > 0 then 
b __LOC__ true;;
#end


#if 1 < 0 then 
b __LOC__ false;;
#end

#if 0 > 1 then 
b __LOC__ false;;
#end


#if 0 < 1 then 
b __LOC__ true;;
#end


let () = Mt.from_pair_suites __MODULE__ !suites