

open Js_promise

let assert_bool b =
  if b then ()
  else
    raise (Invalid_argument "Assertion Failure.")

let fail _ =
  (* assert_bool false *)
  assert false 

let thenTest () =
  let p = resolve 4 in
  p |> then_ (fun x -> resolve @@ assert_bool (x = 4))

let andThenTest () =
  let p = resolve 6 in
  p |> then_ (fun _ -> resolve (12))
    |> then_ (fun y -> resolve @@ assert_bool (y = 12))

let catchTest () =
  let p = reject "error" in
  p |> then_ fail
    |> catch (fun error -> resolve @@ assert_bool (Obj.magic error == "error"))

let orResolvedTest () =
  let p = resolve 42 in
  p |> catch (fun _ -> resolve 22)
    |> then_ (fun value -> resolve @@ assert_bool (value = 42))
    |> catch fail

let orRejectedTest () =
  let p = reject "error" in
  p |> catch (fun _ -> resolve 22)
    |> then_ (fun value -> resolve @@ assert_bool (value = 22))
    |> catch fail

let orElseResolvedTest () =
  let p = resolve 42 in
  p |> catch (fun _ -> resolve 22)
    |> then_ (fun value -> resolve @@ assert_bool (value = 42))
    |> catch fail

let orElseRejectedResolveTest () =
  let p = reject "error" in
  p |> catch (fun _ -> resolve 22)
    |> then_ (fun value -> resolve @@ assert_bool (value = 22))
    |> catch fail

let orElseRejectedRejectTest () =
  let p = reject "error" in
  p |> catch (fun _ -> reject "error 2")
    |> then_ fail
    |> catch (fun error -> resolve @@ assert_bool (Obj.magic error = "error 2"))

let resolveTest () =
  let p1 = resolve 10 in
  p1 |> then_ (fun x -> resolve @@ assert_bool (x = 10))

let rejectTest () =
  let p = reject "error" in
  p |> catch (fun error -> resolve @@ assert_bool (Obj.magic error = "error"))

let thenCatchChainResolvedTest () =
  let p = resolve 20 in
  p |> then_ (fun value -> resolve @@ assert_bool (value = 20) )
    |> catch fail

let thenCatchChainRejectedTest () =
  let p = reject "error" in
  p |> then_ fail
    |> catch (fun error -> resolve @@ assert_bool (Obj.magic error = "error"))

let h = resolve ()
let allResolvedTest () =
  let p1 = resolve 1 in
  let p2 = resolve 2 in
  let p3 = resolve 3 in
  let promises = [| p1; p2; p3 |] in
  all promises
  >>=
  (fun resolved ->
     assert_bool (resolved.(0) = 1) ;
     assert_bool (resolved.(1) = 2) ;
     assert_bool (resolved.(2) = 3) ;
     h
  )
  

let allRejectTest () =
  let p1 = resolve 1 in
  let p2 = resolve 3 in
  let p3 = reject "error" in
  let promises = [| p1; p2; p3 |] in
  all promises
    |> then_ fail
    |> catch (fun error -> assert_bool (Obj.magic error = "error") ; h)

let raceTest () =
  let p1 = resolve "first" in
  let p2 = resolve "second" in
  let p3 = resolve "third" in
  let promises = [| p1; p2; p3 |] in
  race promises
  >>= (fun resolved -> h)
  |> catch fail

let createPromiseRejectTest () =
  make (fun _ reject -> reject "error")
  |> catch (fun error -> assert_bool (Obj.magic error = "error"); h)

let createPromiseFulfillTest () =
  make (fun resolve _ -> resolve "success")
  >>= (fun resolved -> assert_bool (Obj.magic resolved = "success"); h)
  |> catch fail

let () =
  ignore @@ thenTest ();
  ignore @@ andThenTest ();
  ignore @@ catchTest ();
  ignore @@ orResolvedTest ();
  ignore @@ orRejectedTest ();
  ignore @@ orElseResolvedTest ();
  ignore @@ orElseRejectedResolveTest ();
  ignore @@ orElseRejectedRejectTest ();
  ignore @@ thenCatchChainResolvedTest ();
  ignore @@ thenCatchChainRejectedTest ();
  ignore @@ allResolvedTest ();
  ignore @@ allRejectTest ();
  ignore @@ raceTest ();
  ignore @@ createPromiseRejectTest ();
  ignore @@ createPromiseFulfillTest ();
