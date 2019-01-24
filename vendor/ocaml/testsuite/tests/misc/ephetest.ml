let debug = false

open Printf
open Ephemeron

let is_true test s b = printf "%s %s: %s\n" test s (if b then "OK" else "FAIL")
let is_false test s b = is_true test s (not b)

let is_data_value test eph (v:int) =
  match K1.get_data_copy eph with
  | Some x ->
      if !x = v
      then printf "%s data set: OK\n" test
      else printf "%s data set: FAIL(bad value %i)\n" test (!x)
  | None -> printf "%s data set: FAIL\n" test

let is_key_value test eph (v:int) =
  match K1.get_key_copy eph with
  | Some x ->
      if !x = v
      then printf "%s key set: OK\n" test
      else printf "%s key set: FAIL(bad value %i)\n" test (!x)
  | None -> printf "%s key unset: FAIL\n" test

let is_key_unset test eph =
  is_false test "key unset" (K1.check_key eph)

let is_data_unset test eph =
  is_false test "data unset" (K1.check_data eph)

let make_ra () = ref (ref 1) [@@inline never]
let make_rb () = ref (ref (ref 2)) [@@inline never]
let ra = make_ra ()
let rb = make_rb ()

(** test: key alive data dangling *)
let test1 () =
  let test = "test1" in
  Gc.minor ();
  Gc.full_major ();
  let eph : (int ref, int ref) K1.t = K1.create () in
  K1.set_key eph (!ra);
  K1.set_data eph (ref 42);
  is_key_value test eph 1;
  is_data_value test eph 42;
  Gc.minor ();
  is_key_value test eph 1;
  is_data_value test eph 42;
  Gc.full_major ();
  is_key_value test eph 1;
  is_data_value test eph 42;
  ra := ref 12;
  Gc.full_major ();
  is_key_unset test eph;
  is_data_unset test eph
let () = (test1 [@inlined never]) ()

(** test: key dangling data dangling *)
let test2 () =
  let test = "test2" in
  Gc.minor ();
  Gc.full_major ();
  let eph : (int ref, int ref) K1.t = K1.create () in
  K1.set_key eph (ref 125);
  K1.set_data eph (ref 42);
  is_key_value test eph 125;
  is_data_value test eph 42;
  ra := ref 13;
  Gc.minor ();
  is_key_unset test eph;
  is_data_unset test eph
let () = (test2 [@inlined never]) ()

(** test: key dangling data alive *)
let test3 () =
  let test = "test3" in
  Gc.minor ();
  Gc.full_major ();
  let eph : (int ref, int ref) K1.t = K1.create () in
  K1.set_key eph (ref 125);
  K1.set_data eph (!ra);
  is_key_value test eph 125;
  is_data_value test eph 13;
  ra := ref 14;
  Gc.minor ();
  is_key_unset test eph;
  is_data_unset test eph
let () = (test3 [@inlined never]) ()

(** test: key alive but one away, data dangling *)
let test4 () =
  let test = "test4" in
  Gc.minor ();
  Gc.full_major ();
  let eph : (int ref, int ref) K1.t = K1.create () in
  rb := ref (ref 3);
  K1.set_key eph (!(!rb));
  K1.set_data eph (ref 43);
  is_key_value test eph 3;
  is_data_value test eph 43;
  Gc.minor ();
  Gc.minor ();
  is_key_value test eph 3;
  is_data_value test eph 43
let () = (test4 [@inlined never]) ()

(** test: key dangling but one away, data dangling *)
let test5 () =
  let test = "test5" in
  Gc.minor ();
  Gc.full_major ();
  let eph : (int ref, int ref) K1.t = K1.create () in
  rb := ref (ref 3);
  K1.set_key eph (!(!rb));
  K1.set_data eph (ref 43);
  is_key_value test eph 3;
  is_data_value test eph 43;
  !rb := ref 4;
  Gc.minor ();
  Gc.minor ();
  is_key_unset test eph;
  is_data_unset test eph
let () = (test5 [@inlined never]) ()

(** test: key accessible from data but all dangling *)
let test6 () =
  let test = "test6" in
  Gc.minor ();
  Gc.full_major ();
  let eph : (int ref, int ref ref) K1.t = K1.create () in
  rb := ref (ref 3);
  K1.set_key eph (!(!rb));
  K1.set_data eph (ref (!(!rb)));
  Gc.minor ();
  is_key_value test eph 3;
  !rb := ref 4;
  Gc.full_major ();
  is_key_unset test eph;
  is_data_unset test eph
let () = (test6 [@inlined never]) ()

(** test: ephemeron accessible from data but they are dangling *)
type t =
  | No
  | Ephe of (int ref, t) K1.t

let rc = ref No

let test7 () =
  let test = "test7" in
  Gc.minor ();
  Gc.full_major ();
  ra := ref 42;
  let weak : t Weak.t = Weak.create 1 in
  let eph : (int ref, t) K1.t ref = ref (K1.create ()) in
  rc := Ephe !eph;
  Weak.set weak 0 (Some !rc);
  K1.set_key !eph !ra;
  K1.set_data !eph !rc;
  Gc.minor ();
  is_true test "before" (Weak.check weak 0);
  eph := K1.create ();
  rc := No;
  Gc.full_major ();
  Gc.full_major ();
  Gc.full_major ();
  is_false test "after" (Weak.check weak 0)
let () = (test7 [@inlined never]) ()
