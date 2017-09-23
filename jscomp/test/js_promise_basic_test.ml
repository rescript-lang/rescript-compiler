let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = 
  incr test_id ; 
  suites := 
    (loc ^" id " ^ (string_of_int !test_id), (fun _ -> Mt.Eq(x,y))) :: !suites



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

let h = resolve ()

let assertIsNotFound (x : Js_promise.error) = 
  match (function [@bs.open]
  | Not_found -> 0) x with 
  | Some _ -> h
  | _ -> assert false 

(** would be nice to have [%bs.open? Stack_overflow]*)
let catchTest () =
  let p = reject Not_found in
  p |> then_ fail
    |> catch (fun error -> 
      assertIsNotFound error
    )

let orResolvedTest () =
  let p = resolve 42 in
  p |> catch (fun _ -> resolve 22)
    |> then_ (fun value -> resolve @@ assert_bool (value = 42))
    |> catch fail

let orRejectedTest () =
  let p = reject Not_found in
  p |> catch (fun _ -> resolve 22)
    |> then_ (fun value -> resolve @@ assert_bool (value = 22))
    |> catch fail

let orElseResolvedTest () =
  let p = resolve 42 in
  p |> catch (fun _ -> resolve 22)
    |> then_ (fun value -> resolve @@ assert_bool (value = 42))
    |> catch fail

let orElseRejectedResolveTest () =
  let p = reject Not_found in
  p |> catch (fun _ -> resolve 22)
    |> then_ (fun value -> resolve @@ assert_bool (value = 22))
    |> catch fail

let orElseRejectedRejectTest () =
  let p = reject Not_found in
  p |> catch (fun _ -> reject Stack_overflow)
    |> then_ fail
    |> catch (fun error ->
        match (function [@bs.open] Stack_overflow -> 0) error with 
        | Some _ -> h 
        | None -> assert false 
        (* resolve @@ assert_bool (Obj.magic error == Stack_overflow) *))

let resolveTest () =
  let p1 = resolve 10 in
  p1 |> then_ (fun x -> resolve @@ assert_bool (x = 10))

let rejectTest () =
  let p = reject Not_found in
  p |> catch
    (fun error ->
       assertIsNotFound error
       (* resolve @@ assert_bool (Obj.magic error == Not_found) *)
    )

let thenCatchChainResolvedTest () =
  let p = resolve 20 in
  p |> then_ (fun value -> resolve @@ assert_bool (value = 20) )
    |> catch fail

let thenCatchChainRejectedTest () =
  let p = reject Not_found in
  p |> then_ fail
    |> catch (fun error -> 
      assertIsNotFound error
      (* resolve @@ assert_bool (Obj.magic error == Not_found) *))


let allResolvedTest () =
  let p1 = resolve 1 in
  let p2 = resolve 2 in
  let p3 = resolve 3 in
  let promises = [| p1; p2; p3 |] in
  all promises
  |> then_
  (fun resolved ->
     assert_bool (resolved.(0) = 1) ;
     assert_bool (resolved.(1) = 2) ;
     assert_bool (resolved.(2) = 3) ;
     h
  )
  

let allRejectTest () =
  let p1 = resolve 1 in
  let p2 = resolve 3 in
  let p3 = reject Not_found in
  let promises = [| p1; p2; p3 |] in
  all promises
    |> then_ fail
    |> catch (fun error -> assert_bool (Obj.magic error == Not_found) ; h)

let raceTest () =
  let p1 = resolve "first" in
  let p2 = resolve "second" in
  let p3 = resolve "third" in
  let promises = [| p1; p2; p3 |] in
  race promises
  |> then_ (fun resolved -> h)
  |> catch fail

let createPromiseRejectTest () =
  make (fun ~resolve ~reject -> reject Not_found [@bs])
  |> catch (fun error -> assert_bool (Obj.magic error == Not_found); h)

let createPromiseFulfillTest () =
  make (fun ~resolve ~reject:_  -> resolve "success" [@bs])
  |> then_ (fun resolved -> assert_bool (Obj.magic resolved = "success"); h)
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
  ignore @@ createPromiseFulfillTest ()

(** TODO: async tests?
*)
let () = 
    (Js.Promise.all2 (Js.Promise.resolve 2, Js.Promise.resolve 3))
    |> Js.Promise.then_ (fun (a,b) -> 
    eq __LOC__ (a,b) (2,3); 
    
    Js.Promise.resolve ()
    )
    |> ignore


;; Js.log (List.length !suites)     
     
;; Js.log "hey"
;; Mt.from_pair_suites __FILE__ !suites

let twop = Js.Promise.resolve 2 
let then_ = Js.Promise.then_
let re = Js.Promise.resolve

;; Mt.from_promise_suites __FILE__
  [
    __LOC__, 
    twop
    |> then_ (fun x -> re @@ Mt.Eq(x,2));
    __LOC__, 
    twop
    |> then_ (fun x -> re @@ Mt.Neq(x,3))        
  ]