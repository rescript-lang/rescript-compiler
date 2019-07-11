let for_3 x =
  let v = ref 0 in
  let arr = Array.map (fun _ _ -> ()) x in
  for i = 0 to Array.length x - 1 do
    let j = i * 2 in
    arr.(i) <- (fun _ -> v := !v + j)
  done ;
  Array.iter (fun x -> x ()) arr ;
  !v

let for_4 x =
  let v = ref 0 in
  let arr = Array.map (fun _ _ -> ()) x in
  for i = 0 to Array.length x - 1 do
    let j = i * 2 in
    let k = 2 * j in
    arr.(i) <- (fun _ -> v := !v + k)
  done ;
  Array.iter (fun x -> x ()) arr ;
  !v

let for_5 x u =
  let v = ref 0 in
  let arr = Array.map (fun _ _ -> ()) x in
  for i = 0 to Array.length x - 1 do
    let _j = i * 2 in
    let k = 2 * u * u in
    arr.(i) <- (fun _ -> v := !v + k)
  done ;
  Array.iter (fun x -> x ()) arr ;
  !v

let for_6 x u =
  let v = ref 0 in
  let arr = Array.map (fun _ _ -> ()) x in
  let v4 = ref 0 in
  let v5 = ref 0 in
  let inspect_3 = ref (-1) in
  incr v4 ;
  for j = 0 to 1 do
    incr v5 ;
    let v2 = ref 0 in
    let v3 = u in
    for i = 0 to Array.length x - 1 do
      let _j = i * 2 in
      let k = 2 * u * u in
      let h = 2 * !v5 in
      incr v2 ;
      arr.(i) <- (fun _ -> v := !v + k + !v2 + !v4 + !v5 + h + v3)
      (* v2 should not be captured *)
    done ;
    inspect_3 := !v2
  done ;
  Array.iter (fun x -> x ()) arr ;
  [|!v; !v4; !v5; !inspect_3|]

let for_7 () =
  let i_len = 7 in
  let j_len = 3 in
  let v = ref 0 in
  let arr = Array.make (i_len * j_len) (fun _ -> ()) in
  for i = 0 to i_len - 1 do
    for j = 0 to j_len - 1 do
      arr.((i * j_len) + j) <-
        (fun _ ->
          (* print_endline @@ Printf.sprintf "%d %d" i j; *)
          (* print_endline @@ string_of_int ( i * j_len + j) ^ " "  *)
          (* ^ (string_of_int i) ^ " " ^ string_of_int j; *)
          v := !v + i + j)
    done
  done ;
  Array.iter (fun f -> f ()) arr ;
  !v

let for_8 () =
  let i_len = 7 in
  let j_len = 3 in
  let v = ref 0 in
  let arr = Array.make (i_len * j_len) (fun _ -> ()) in
  for i = 0 to i_len - 1 do
    let k = 2 * i in
    for j = 0 to j_len - 1 do
      let h = i + j in
      arr.((i * j_len) + j) <-
        (fun _ ->
          (* prerr_endline @@ Printf.sprintf "%d %d" i j; *)
          v := !v + i + j + h + k)
    done
  done ;
  Array.iter (fun f -> f ()) arr ;
  !v

let for_9 () =
  let collect, get =
    let v : int list ref = ref [] in
    ((fun x -> v := x :: !v), fun () -> Array.of_list @@ List.rev !v)
  in
  let i_len = 2 in
  let j_len = 2 in
  let vv = ref 0 in
  let vv2 = ref 0 in
  let arr = Array.make (i_len * j_len) (fun _ -> ()) in
  let arr2 = Array.make i_len (fun _ -> ()) in
  for i = 0 to i_len - 1 do
    let v = ref 0 in
    (* incr v ;  *)
    v := !v + i ;
    for j = 0 to j_len - 1 do
      incr v ;
      collect !v ;
      arr.((i * j_len) + j) <-
        (fun _ ->
          (* prerr_endline @@ Printf.sprintf "<%d" !vv ;  *)
          (* prerr_endline @@ Printf.sprintf ">%d" !vv ;  *)
          vv := !vv + !v)
      (* v should not be captured inside , 
           since for next iteration, 
           we are bound the same v

           there are four iterations of this function
           
           the first two bound one v 

           the second two bound the other one

           -- sometimes it's hard to tell the difference,  
           when v is not relevant to the outer [index]
           actually we have to lexical scope the whole for statement
         *)
    done ;
    arr2.(i) <- (fun _ -> vv2 := !vv2 + !v)
    (* v should be captured, since next iteration 
        v is changed
      *)
  done ;
  Array.iter (fun f -> f ()) arr ;
  Array.iter (fun f -> f ()) arr2 ;
  [|(!vv, get (), !vv2)|]

(**

See how google closure works, in both simple model and advanced model

{[
var x = []

var u = []
var result = 0
for(let i = 0; i < 2; ++i){
  let counter = 0;
  counter += i;
  for(let j = 0; j < 2 ; ++j){
     x[i * 2 +j ] = ()=>{ result += counter}
   }
  u.push(counter)
}
x.forEach(x=>x())
console.log(result,u)
]}


*)

let suites =
  [ ("for_loop_test_3", fun _ -> Mt.Eq (90, for_3 @@ Array.make 10 2))
  ; ("for_loop_test_4", fun _ -> Mt.Eq (180, for_4 @@ Array.make 10 2))
  ; ("for_loop_test_5", fun _ -> Mt.Eq (2420, for_5 (Array.make 10 2) 11))
  ; ( "for_loop_test_6"
    , fun _ -> Mt.Eq ([|30; 1; 2; 3|], for_6 (Array.make 3 0) 0) )
  ; ("for_loop_test_7", fun _ -> Mt.Eq (84, for_7 ()))
  ; ("for_loop_test_8", fun _ -> Mt.Eq (294, for_8 ()))
  ; ("for_loop_test_9", fun _ -> Mt.Eq ([|(10, [|1; 2; 2; 3|], 5)|], for_9 ()))
  ]
