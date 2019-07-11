let for_ x =
  for i = 0 to print_endline "hi" ; ignore 3 ; Array.length x do
    print_endline x.(i)
  done

let for_2 x =
  for i = 0 to Array.length x do
    print_endline x.(i)
  done

let for_3 x =
  let v = ref 0 in
  let arr = Array.map (fun _ _ -> ()) x in
  for i = 0 to Array.length x do
    let j = i * 2 in
    arr.(i) <- (fun _ -> v := !v + j)
  done ;
  Array.iter (fun x -> x ()) arr ;
  !v

let for_4 x =
  let v = ref 0 in
  let arr = Array.map (fun _ _ -> ()) x in
  for i = 0 to Array.length x do
    let j = i * 2 in
    let k = 2 * j in
    arr.(i) <- (fun _ -> v := !v + k)
  done ;
  Array.iter (fun x -> x ()) arr ;
  !v

let for_5 x u =
  let v = ref 0 in
  let arr = Array.map (fun _ _ -> ()) x in
  for i = 0 to Array.length x do
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
  incr v4 ;
  for j = 0 to 1 do
    incr v5 ;
    let v2 = ref 0 in
    let v3 = u in
    for i = 0 to Array.length x do
      let _j = i * 2 in
      let k = 2 * u * u in
      let h = 2 * !v5 in
      incr v2 ;
      arr.(i) <- (fun _ -> v := !v + k + !v2 + v3 + !v4 + !v5 + h)
    done
  done ;
  Array.iter (fun x -> x ()) arr ;
  !v
