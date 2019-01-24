external ( + ) : int64 -> int64 -> int64
  = "" "test_int64_add" [@@noalloc] [@@unboxed]
external ( - ) : int64 -> int64 -> int64
  = "" "test_int64_sub" [@@noalloc] [@@unboxed]
external ( * ) : int64 -> int64 -> int64
  = "" "test_int64_mul" [@@noalloc] [@@unboxed]

external ignore_int64 : (int64 [@unboxed]) -> unit
  = "" "test_ignore_int64" [@@noalloc]

let f () =
  let r = ref 1L in
  for i = 0 to 100000 do
    let n = !r + !r in
    r := n * n
  done;
  ignore_int64 !r

let () =
  let a0 = Gc.allocated_bytes () in
  let a1 = Gc.allocated_bytes () in
  let _x = f () in
  let a2 = Gc.allocated_bytes () in
  let alloc = (a2 -. 2. *. a1 +. a0) in
  assert(alloc = 0.)
