module M = Map.Make(struct type t = int let compare (x:t) y = compare x y end)

let img x m = try Some(M.find x m) with Not_found -> None

let testvals = [0;1;2;3;4;5;6;7;8;9]

let check msg cond =
  if not (List.for_all cond testvals) then
    Printf.printf "Test %s FAILED\n%!" msg

let checkbool msg b =
  if not b then
    Printf.printf "Test %s FAILED\n%!" msg

let uncurry (f: 'a -> 'b -> 'c) (x, y: 'a * 'b) : 'c = f x y

let test x v s1 s2 =

  checkbool "is_empty"
    (M.is_empty s1 = List.for_all (fun i -> img i s1 = None) testvals);

  check "mem"
    (fun i -> M.mem i s1 = (img i s1 <> None));

  check "add"
    (let s = M.add x v s1 in
     fun i -> img i s = (if i = x then Some v else img i s1));

  check "singleton"
    (let s = M.singleton x v in
     fun i -> img i s = (if i = x then Some v else None));

  check "remove"
    (let s = M.remove x s1 in
     fun i -> img i s = (if i = x then None else img i s1));

  check "merge-union"
    (let f _ o1 o2 =
       match o1, o2 with
       | Some v1, Some v2 -> Some (v1 +. v2)
       | None, _ -> o2
       | _, None -> o1 in
     let s = M.merge f s1 s2 in
     fun i -> img i s = f i (img i s1) (img i s2));

  check "merge-inter"
    (let f _ o1 o2 =
       match o1, o2 with
       | Some v1, Some v2 -> Some (v1 -. v2)
       | _, _ -> None in
     let s = M.merge f s1 s2 in
     fun i -> img i s = f i (img i s1) (img i s2));

  checkbool "bindings"
    (let rec extract = function
       | [] -> []
       | hd :: tl ->
           match img hd s1 with
           | None -> extract tl
           | Some v ->(hd,  v) :: extract tl in
     M.bindings s1 = extract testvals);

  checkbool "for_all"
    (let p x y = x mod 2 = 0 in
     M.for_all p s1 = List.for_all (uncurry p) (M.bindings s1));

  checkbool "exists"
    (let p x y = x mod 3 = 0 in
     M.exists p s1 = List.exists (uncurry p) (M.bindings s1));

  checkbool "filter"
    (let p x y = x >= 3 && x <= 6 in
     M.bindings(M.filter p s1) = List.filter (uncurry p) (M.bindings s1));

  checkbool "partition"
    (let p x y = x >= 3 && x <= 6 in
     let (st,sf) = M.partition p s1
     and (lt,lf) = List.partition (uncurry p) (M.bindings s1) in
     M.bindings st = lt && M.bindings sf = lf);

  checkbool "cardinal"
    (M.cardinal s1 = List.length (M.bindings s1));

  checkbool "min_binding"
    (try
       let (k,v) = M.min_binding s1 in
       img k s1 = Some v && M.for_all (fun i _ -> k <= i) s1
     with Not_found ->
       M.is_empty s1);

  checkbool "max_binding"
    (try
       let (k,v) = M.max_binding s1 in
       img k s1 = Some v && M.for_all (fun i _ -> k >= i) s1
     with Not_found ->
       M.is_empty s1);

  checkbool "choose"
    (try
       let (x,v) = M.choose s1 in img x s1 = Some v
     with Not_found ->
       M.is_empty s1);

  checkbool "find_first"
    (let (l, p, r) = M.split x s1 in
    if p = None && M.is_empty r then
      try
        let _ = M.find_first (fun k -> k >= x) s1 in
        false
      with Not_found ->
        true
    else
      let (k, v) = M.find_first (fun k -> k >= x) s1 in
      match p with
        None -> (k, v) = M.min_binding r
      | Some v1 -> (k, v) = (x, v1));

  checkbool "find_first_opt"
    (let (l, p, r) = M.split x s1 in
    let find_first_opt_result = M.find_first_opt (fun k -> k >= x) s1 in
    if p = None && M.is_empty r then
      match find_first_opt_result with
        None -> true
      | _ -> false
    else
      match find_first_opt_result with
        | None -> false
        | Some (k, v) ->
          (match p with
          | None -> (k, v) = M.min_binding r
          | Some v1 -> (k, v) = (x, v1)));

  checkbool "find_last"
    (let (l, p, r) = M.split x s1 in
    if p = None && M.is_empty l then
      try
        let _ = M.find_last (fun k -> k <= x) s1 in
        false
      with Not_found ->
        true
    else
      let (k, v) = M.find_last (fun k -> k <= x) s1 in
      match p with
        None -> (k, v) = M.max_binding l
      | Some v1 -> (k, v) = (x, v1));

  checkbool "find_last_opt"
    (let (l, p, r) = M.split x s1 in
    let find_last_opt_result = M.find_last_opt (fun k -> k <= x) s1 in
    if p = None && M.is_empty l then
      match find_last_opt_result with
        None -> true
      | _ -> false
    else
      (match find_last_opt_result with
      | None -> false
      | Some (k, v) ->
        (match p with
        | None -> (k, v) = M.max_binding l
        | Some v1 -> (k, v) = (x, v1))));

  check "split"
    (let (l, p, r) = M.split x s1 in
     fun i ->
       if i < x then img i l = img i s1
       else if i > x then img i r = img i s1
       else p = img i s1)

let rkey() = Random.int 10

let rdata() = Random.float 1.0

let rmap() =
  let s = ref M.empty in
  for i = 1 to Random.int 10 do s := M.add (rkey()) (rdata()) !s done;
  !s

let _ =
  Random.init 42;
  for i = 1 to 10000 do test (rkey()) (rdata()) (rmap()) (rmap()) done

let () =
  (* check that removing a binding from a map that is not present in this map
     (1) doesn't allocate and (2) return the original map *)
  let m1 = ref M.empty in
  for i = 1 to 10 do m1 := M.add i (float i) !m1 done;
  let m2 = ref !m1 in

  let a0 = Gc.allocated_bytes () in
  let a1 = Gc.allocated_bytes () in
  for i = 11 to 30 do m2 := M.remove i !m2 done;
  let a2 = Gc.allocated_bytes () in

  assert (!m2 == !m1);
  assert(a2 -. a1 = a1 -. a0)

let () =
  (* check that filtering a map where all bindings are satisfied by
     the given predicate returns the original map *)
  let m1 = ref M.empty in
  for i = 1 to 10 do m1 := M.add i (float i) !m1 done;
  let m2 = M.filter (fun e _ -> e >= 0) !m1 in
  assert (m2 == !m1)

let () =
  (* check that adding a binding "x -> y" to a map that already
     contains it doesn't allocate and return the original map. *)
  let m1 = ref M.empty in
  let tmp = ref None in
  for i = 1 to 10 do
    tmp := Some (float i);
    m1 := M.add i !tmp !m1
  done;
  let m2 = ref !m1 in

  let a0 = Gc.allocated_bytes () in
  let a1 = Gc.allocated_bytes () in

  (* 10 |-> !tmp is already present in !m2 *)
  m2 := M.add 10 !tmp !m2;

  let a2 = Gc.allocated_bytes () in

  assert (!m2 == !m1);
  assert(a2 -. a1 = a1 -. a0);

  (* 4 |-> Some 84. is not present in !m2 *)
  m2 := M.add 4 (Some 84.) !m2;

  assert (not (!m2 == !m1));
