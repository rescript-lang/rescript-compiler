
let check f n =
  assert (
    try ignore ((Sys.opaque_identity f) n); false with
      Division_by_zero -> true
  )

let div_int n  = n / 0
let div_int32 n = Int32.div n 0l
let div_int64 n = Int64.div n 0L
let div_nativeint n = Nativeint.div n 0n

let mod_int n  = n mod 0
let mod_int32 n = Int32.rem n 0l
let mod_int64 n = Int64.rem n 0L
let mod_nativeint n = Nativeint.rem n 0n

let div_int_opaque n  = n / (Sys.opaque_identity 0)
let div_int32_opaque n = Int32.div n (Sys.opaque_identity 0l)
let div_int64_opaque n = Int64.div n (Sys.opaque_identity 0L)
let div_nativeint_opaque n = Nativeint.div n (Sys.opaque_identity 0n)

let mod_int_opaque n  = n mod (Sys.opaque_identity 0)
let mod_int32_opaque n = Int32.rem n (Sys.opaque_identity 0l)
let mod_int64_opaque n = Int64.rem n (Sys.opaque_identity 0L)
let mod_nativeint_opaque n = Nativeint.rem n (Sys.opaque_identity 0n)

let () =
  check div_int 33;
  check div_int 0;
  check div_int32 33l;
  check div_int32 0l;
  check div_int64 33L;
  check div_int64 0L;
  check div_nativeint 33n;
  check div_nativeint 0n;

  check mod_int 33;
  check mod_int 0;
  check mod_int32 33l;
  check mod_int32 0l;
  check mod_int64 33L;
  check mod_int64 0L;
  check mod_nativeint 33n;
  check mod_nativeint 0n;

  check div_int_opaque 33;
  check div_int_opaque 0;
  check div_int32_opaque 33l;
  check div_int32_opaque 0l;
  check div_int64_opaque 33L;
  check div_int64_opaque 0L;
  check div_nativeint_opaque 33n;
  check div_nativeint_opaque 0n;

  check mod_int_opaque 33;
  check mod_int_opaque 0;
  check mod_int32_opaque 33l;
  check mod_int32_opaque 0l;
  check mod_int64_opaque 33L;
  check mod_int64_opaque 0L;
  check mod_nativeint_opaque 33n;
  check mod_nativeint_opaque 0n;
  ()

let () =
  print_endline "***** OK *****"
