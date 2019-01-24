module S = Set.Make(struct type t = int let compare (x:t) y = compare x y end)

let testvals = [0;1;2;3;4;5;6;7;8;9]

let check msg cond =
  if not (List.for_all cond testvals) then
    Printf.printf "Test %s FAILED\n%!" msg

let checkbool msg b =
  if not b then
    Printf.printf "Test %s FAILED\n%!" msg

let normalize_cmp c =
  if c = 0 then 0 else if c > 0 then 1 else -1

let test x s1 s2 =

  checkbool "is_empty"
    (S.is_empty s1 = List.for_all (fun i -> not (S.mem i s1)) testvals);

  check "add"
    (let s = S.add x s1 in
     fun i -> S.mem i s = (S.mem i s1 || i = x));

  check "singleton"
    (let s = S.singleton x in
     fun i -> S.mem i s = (i = x));

  check "remove"
    (let s = S.remove x s1 in
     fun i -> S.mem i s = (S.mem i s1 && i <> x));

  check "union"
    (let s = S.union s1 s2 in
     fun i -> S.mem i s = (S.mem i s1 || S.mem i s2));

  check "inter"
    (let s = S.inter s1 s2 in
     fun i -> S.mem i s = (S.mem i s1 && S.mem i s2));

  check "diff"
    (let s = S.diff s1 s2 in
     fun i -> S.mem i s = (S.mem i s1 && not (S.mem i s2)));

  checkbool "elements"
    (S.elements s1 = List.filter (fun i -> S.mem i s1) testvals);

  checkbool "compare"
    (normalize_cmp (S.compare s1 s2)
     = normalize_cmp (compare (S.elements s1) (S.elements s2)));

  checkbool "equal"
    (S.equal s1 s2 = (S.elements s1 = S.elements s2));

  check "subset"
    (let b = S.subset s1 s2 in
     fun i -> if b && S.mem i s1 then S.mem i s2 else true);

  checkbool "subset2"
    (let b = S.subset s1 s2 in
     b || not (S.is_empty (S.diff s1 s2)));

  checkbool "map"
    (S.elements (S.map succ s1) = List.map succ (S.elements s1));

  checkbool "map2"
    (S.map (fun x -> x) s1 == s1);

  checkbool "map3"
    ((* check that the traversal is made in increasing element order *)
     let last = ref min_int in
     S.map (fun x -> assert (!last <= x); last := x; x) s1 == s1);

  checkbool "for_all"
    (let p x = x mod 2 = 0 in
     S.for_all p s1 = List.for_all p (S.elements s1));

  checkbool "exists"
    (let p x = x mod 3 = 0 in
     S.exists p s1 = List.exists p (S.elements s1));

  checkbool "filter"
    (let p x = x >= 3 && x <= 6 in
     S.elements(S.filter p s1) = List.filter p (S.elements s1));

  checkbool "partition"
    (let p x = x >= 3 && x <= 6 in
     let (st,sf) = S.partition p s1
     and (lt,lf) = List.partition p (S.elements s1) in
     S.elements st = lt && S.elements sf = lf);

  checkbool "cardinal"
    (S.cardinal s1 = List.length (S.elements s1));

  checkbool "min_elt"
    (try
       let m = S.min_elt s1 in
       S.mem m s1 && S.for_all (fun i -> m <= i) s1
     with Not_found ->
       S.is_empty s1);

  checkbool "max_elt"
    (try
       let m = S.max_elt s1 in
       S.mem m s1 && S.for_all (fun i -> m >= i) s1
     with Not_found ->
       S.is_empty s1);

  checkbool "choose"
    (try
       let x = S.choose s1 in S.mem x s1
     with Not_found ->
       S.is_empty s1);

  checkbool "find_first"
    (let (l, p, r) = S.split x s1 in
    if not p && S.is_empty r then
      try
        let _ = S.find_first (fun k -> k >= x) s1 in
        false
      with Not_found ->
        true
    else
      let e = S.find_first (fun k -> k >= x) s1 in
      if p then
        e = x
      else
        e = S.min_elt r);

  checkbool "find_first_opt"
    (let (l, p, r) = S.split x s1 in
    let find_first_opt_result = S.find_first_opt (fun k -> k >= x) s1 in
    if not p && S.is_empty r then
      match find_first_opt_result with
        None -> true
      | _ -> false
    else
      (match find_first_opt_result with
      | None -> false
      | Some e -> if p then e = x else e = S.min_elt r));

  checkbool "find_last"
    (let (l, p, r) = S.split x s1 in
    if not p && S.is_empty l then
      try
        let _ = S.find_last (fun k -> k <= x) s1 in
        false
      with Not_found ->
        true
    else
      let e = S.find_last (fun k -> k <= x) s1 in
      if p then
        e = x
      else
        e = S.max_elt l);

  checkbool "find_last_opt"
    (let (l, p, r) = S.split x s1 in
    let find_last_opt_result = S.find_last_opt (fun k -> k <= x) s1 in
    if not p && S.is_empty l then
      match find_last_opt_result with
        None -> true
      | _ -> false
    else
      (match find_last_opt_result with
      | None -> false
      | Some e -> if p then e = x else e = S.max_elt l));

  check "split"
    (let (l, p, r) = S.split x s1 in
     fun i ->
       if i < x then S.mem i l = S.mem i s1
       else if i > x then S.mem i r = S.mem i s1
       else p = S.mem i s1)

let relt() = Random.int 10

let rset() =
  let s = ref S.empty in
  for i = 1 to Random.int 10 do s := S.add (relt()) !s done;
  !s

let _ =
  Random.init 42;
  for i = 1 to 10000 do test (relt()) (rset()) (rset()) done

let () =
  (* #6645: check that adding an element to set that already contains
     it doesn't allocate and return the original set. *)
  let s1 = ref S.empty in
  for i = 1 to 10 do s1 := S.add i !s1 done;
  let s2 = ref !s1 in

  let a0 = Gc.allocated_bytes () in
  let a1 = Gc.allocated_bytes () in
  for i = 1 to 10 do s2 := S.add i !s2 done;
  let a2 = Gc.allocated_bytes () in

  assert (!s2 == !s1);
  assert(a2 -. a1 = a1 -. a0)

let () =
  (* check that removing an element from a set that is not present in this set
     (1) doesn't allocate and (2) return the original set *)
  let s1 = ref S.empty in
  for i = 1 to 10 do s1 := S.add i !s1 done;
  let s2 = ref !s1 in

  let a0 = Gc.allocated_bytes () in
  let a1 = Gc.allocated_bytes () in
  for i = 11 to 30 do s2 := S.remove i !s2 done;
  let a2 = Gc.allocated_bytes () in

  assert (!s2 == !s1);
  assert(a2 -. a1 = a1 -. a0)

let () =
  (* check that filtering a set where all elements are satisfied by
     the given predicate return the original set *)
  let s1 = ref S.empty in
  for i = 1 to 10 do s1 := S.add i !s1 done;
  let s2 = S.filter (fun e -> e >= 0) !s1 in
  assert (s2 == !s1)

let valid_structure s =
  (* this test should return 'true' for all set,
     but it can detect sets that are ill-structured,
     for example incorrectly ordered, as the S.mem
     function will make assumptions about the set ordering.

     (This trick was used to exhibit the bug in PR#7403)
  *)
  List.for_all (fun n -> S.mem n s) (S.elements s)

let () =
  (* PR#7403: map buggily orders elements according to the input
     set order, not the output set order. Mapping functions that
     change the value ordering thus break the set structure. *)
  let test = S.of_list [1; 3; 5] in
  let f = function 3 -> 8 | n -> n in
  assert (valid_structure (S.map f test))
