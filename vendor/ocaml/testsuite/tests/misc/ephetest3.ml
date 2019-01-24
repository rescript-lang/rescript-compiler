(** This test weak table by application to the memoization of collatz
    (also known as syracuse) algorithm suite computation *)

(** We use Int64 because they are boxed *)

(** number of element of the suite to compute (more are computed) *)
let n = 1000

let two = Int64.of_int 2
let three = Int64.of_int 3

let collatz x =
  if Int64.equal (Int64.rem x two) Int64.zero
  then Int64.div x two
  else Int64.succ (Int64.mul x three)

module S = struct
  include Int64
  let hash (x:t) = Hashtbl.hash x
end

let pp = Int64.to_string

module HW = Ephemeron.K1.Make(S)
module SW = Weak.Make(S)


let sw = SW.create n
let hashcons x = SW.merge sw x

let hw = HW.create n

let rec fill_hw x =
  if not (HW.mem hw x) then begin
    let y = hashcons (collatz x) in
    HW.add hw x y;
    fill_hw y
  end

exception InvariantBroken of string
let test b = Printf.ksprintf (fun s -> if not b then raise (InvariantBroken s))

let rec check_hw_aux cache x =
  (** We use int so that the cache doesn't make x alive *)
  if not (Hashtbl.mem cache (Int64.to_int x)) then begin
    test (HW.mem hw x) "missing %s%!" (pp x);
    let y =
      try HW.find hw x
      with Not_found ->
        test (not (HW.mem hw x)) "key in the table but data missing %s!%!"
          (pp x);
        test false "missing %s%!" (pp x);
        assert false
    in
    let y' = collatz x in
    test (Int64.equal y y') "bad result for %s: %s instead of %s%!"
      (pp x) (pp y) (pp y');
    let y'' = hashcons y' in
    test (y == y'') "bad result for %s: not physically equal%!" (pp x);
    Hashtbl.add cache (Int64.to_int x) ();
    check_hw_aux cache y
  end

let check_hw iter =
  let cache = Hashtbl.create n in
  iter (fun x -> check_hw_aux cache x)

(** tests *)

let run ~next ~check =
  HW.reset hw;
  SW.clear sw;
  (* Gc.full_major (); *)
  for x=0 to n do
    let x' = next x in
    fill_hw x';
    check x;
  done;
  Gc.full_major ();
  HW.clean hw;
  Printf.printf "length: %i\n%!" (HW.length hw)

let print_stats () =
  let print_stats name stats =
    Printf.printf "%s (%3i,%3i,%3i): %!"
      name
      stats.Hashtbl.num_bindings
      stats.Hashtbl.num_buckets
      stats.Hashtbl.max_bucket_length;
    Array.iteri (fun i n -> Printf.printf "%i: %i, %!" i n)
      stats.Hashtbl.bucket_histogram;
    Printf.printf "\n%!";
  in
  print_stats "stats      : " (HW.stats hw);
  print_stats "stats_alive: " (HW.stats_alive hw)

let test_keep_last d d' =
  Printf.printf "## Keep last %i alive, check each %i ##\n%!" (n/d) (n/d');
  let keep_alive = Array.make (n/d) Int64.zero in
  let next x =
    let x' = hashcons (Int64.of_int x) in
    Array.set keep_alive (x mod (n/d)) x';
    x'
  in
  let check x =
    if x mod (n/d') = 0 || x = n then begin
      check_hw (fun f -> Array.iter f keep_alive)
    end
  in
  run ~next ~check;
  (** keep the array alive until the end *)
  let s =
    Array.fold_left (fun acc x -> Int64.add x acc) Int64.zero keep_alive in
  Printf.printf "sum of kept alive %s\n%!" (pp s);
  print_stats ();
  Printf.printf "\n%!"

let () =
  test_keep_last 1 10;
  test_keep_last 50 10;
  test_keep_last 100 2
