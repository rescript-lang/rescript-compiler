let v = "gso"

let is_equal () =
  assert (Bytes.get (Bytes.make 3 'a') 0 = 'a') ;
  assert (Bytes.unsafe_get (Bytes.make 3 'a') 0 = 'a') ;
  let u = Bytes.make 3 'a' in
  Bytes.unsafe_set u 0 'b' ;
  assert (Bytes.unsafe_get u 0 = 'b') ;
  assert (v.[0] = 'g')

let is_exception () = try raise Not_found with Not_found -> ()

let is_normal_exception _x =
  let module E = struct exception A of int end in
  let v = E.A 3 in
  try raise v with E.A 3 -> ()

let is_arbitrary_exception () =
  let module E = struct exception A end in
  try raise E.A with _ -> ()

let suites =
  [ ("is_equal", is_equal)
  ; ("is_exception", is_exception)
  ; ("is_normal_exception", is_normal_exception)
  ; ("is_arbitrary_exception", is_arbitrary_exception) ]

;;
Mt.from_suites "exception" suites
