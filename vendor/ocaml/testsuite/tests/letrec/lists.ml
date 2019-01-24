(* a test with lists, because cyclic lists are fun *)
let test =
  let rec li = 0::1::2::3::4::5::6::7::8::9::li in
  match li with
    | 0::1::2::3::4::5::6::7::8::9::
        0::1::2::3::4::5::6::7::8::9::li' ->
      assert (li == li')
    | _ -> assert false
