let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = Mt.eq_suites ~suites ~test_id loc x y
let b loc v  = Mt.bool_suites ~suites ~test_id loc v

module Icmp =
  (val Belt.Id.comparable
      ~cmp:(fun (x : int) y ->  compare x y)
  )
module M = Belt.Map
module N = Belt.Set

module A = Belt.Array
module I = Array_data_util


let mapOfArray x = M.fromArray ~id:(module Icmp) x
let setOfArray x = N.fromArray ~id:(module Icmp) x
let emptyMap () = M.make (module Icmp)

let mergeInter s1 s2 =
  setOfArray @@ M.keysToArray (M.merge s1 s2 (fun   k v1 v2 ->
      match v1,v2 with
      | Some _, Some _ -> Some ()
      | _, _ -> None
    ))

let mergeUnion s1 s2 =
  setOfArray @@ M.keysToArray @@ M.merge s1 s2 (fun   k v1 v2 ->
      match v1,v2 with
      | None, None -> None
      | _, _ -> Some ()
    )
let mergeDiff s1 s2 =
  setOfArray @@ M.keysToArray @@ M.merge s1 s2 (fun   k v1 v2 ->
      match v1,v2 with
      | Some _, None -> Some ()
      | Some _, Some _
      | None, _ -> None
    )

let randomRange i j =
  A.map (I.randomRange i j) (fun x -> (x,x))

let () =
  let u0 = mapOfArray (randomRange 0 100) in
  let u1 = mapOfArray (randomRange 30 120) in
  b __LOC__ (N.eq (mergeInter u0 u1) (setOfArray (I.range 30 100)));
  b __LOC__ (N.eq (mergeUnion u0 u1) (setOfArray (I.range 0 120)));
  b __LOC__ (N.eq (mergeDiff u0 u1) (setOfArray (I.range 0 29)));
  b __LOC__ (N.eq (mergeDiff u1 u0) (setOfArray (I.range 101 120)))


let () =
  let a0 = mapOfArray (randomRange 0 10) in
  let a1 = M.set a0 3 33 in (* (3,3) *)
  let a2 = M.remove a1 3 in  (* no 3 *)
  let a3 = M.update a2 3 (fun    k ->
      match k with
      | Some k -> Some (k + 1)
      | None  ->  Some 11
    ) in  (* 3, 11 *)
  let a4 = M.update a2 3 (fun    k ->
      match k with
      | Some k-> Some (k + 1)
      | None  ->  None
    ) in  (* no 3 *)
  let a5 = M.remove a0 3 in
  let a6 = M.remove a5 3 in
  b __LOC__ (a5 == a6);
  b __LOC__ (M.has a0 3);
  b __LOC__ (not (M.has a5 3));
  b __LOC__ (Js.eqUndefined 3 (M.getUndefined a0 3));
  b __LOC__ (Js.eqUndefined 33 (M.getUndefined a1 3));
  b __LOC__ ((M.getUndefined a2 3) = Js.undefined);

  b __LOC__ (Js.eqUndefined 11 (M.getUndefined a3 3));
  b __LOC__ ((M.getUndefined a4 3) = Js.undefined);

  let a7 = M.removeMany a0 [|7;8;0;1;3;2;4;922;4;5;6;|] in
  eq __LOC__ (M.keysToArray a7) [|9;10|];
  let a8 = M.removeMany a7 (I.randomRange 0 100) in
  b __LOC__ (M.isEmpty a8)


let () =
  let module Array = M in
  let u0 = mapOfArray (randomRange 0 100) in
  let u1 = u0.(3) <- 32  in
  eq __LOC__ u1.(3) (Some 32);
  eq __LOC__ u0.(3) (Some 3)


let acc m i =
  M.update m i (fun   n -> match n with None -> Some 1 | Some acc -> Some (acc + 1))

let acc m is : _ M.t =
  A.reduce is m (fun a i -> acc a i)

let () =
  let m = emptyMap () in
  let m1 = acc m (A.concat (I.randomRange 0 20) (I.randomRange 10 30)) in
  b __LOC__
  (M.eq m1
  (mapOfArray (A.makeBy 31 (fun i -> i, if i >= 10 && i <= 20 then 2 else 1 )))
  (fun   x y -> x = y)
  )

let () =
  let v0 = emptyMap () in
  let v1 = M.mergeMany v0
  (A.map (I.randomRange 0 10_000)  (fun x -> x , x)) in

  let v2 = mapOfArray
  (A.map (I.randomRange 0 10_000) (fun x -> x, x)) in

  b __LOC__ (M.eq v1 v2 (fun    x y -> x = y ));

  let inc = (fun    x ->
  match x with None -> Some 0
  | Some  v -> Some (v +  1)
  )  in
  let v3 = M.update v1 10 inc in
  let v4 = M.update v3 (-10) inc in
  let (v5, v6), pres = M.split v3 5_000 in
  b __LOC__ (match M.get v3 10 with Some 11 -> true | _ -> false);
  b __LOC__ (match M.get v3 (-10) with None -> true | _ -> false);
  b __LOC__ (match M.get v4 (-10) with Some 0 -> true | _ -> false);
  b __LOC__ (M.isEmpty (M.remove (emptyMap ())  0) );
  b __LOC__ (M.isEmpty (M.removeMany (emptyMap ())  [|0|]) );
  b __LOC__ (match pres with Some 5_000 -> true | _ -> false);
  b __LOC__ (A.eq (M.keysToArray v5) (A.makeBy 5_000 (fun  i -> i))  (=) );
  b __LOC__ (A.eq (M.keysToArray v6) (A.makeBy 5_000 (fun  i -> 5_001 +i))  (=) );

  let v7 = M.remove v3 5_000 in
  let (v8,v9), pres2 = M.split v7 5_000 in
  b __LOC__ (match pres2 with None -> true | _ -> false);
  b __LOC__ (A.eq (M.keysToArray v8) (A.makeBy 5_000 (fun  i -> i))  (=) );
  b __LOC__ (A.eq (M.keysToArray v9) (A.makeBy 5_000 (fun  i -> 5_001 +i))  (=) )



;; Mt.from_pair_suites __FILE__ !suites
