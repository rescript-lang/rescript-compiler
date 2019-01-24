(* This test attempts to check that unused closures are not deleted
   during conversion from flambda to clambda. The idea is that there is
   a direct call to [foo] in [bar] even though the closure for [foo] is
   not used. This requires [bar] to be have a specialised parameter that
   would be [foo]'s closure were there any calls to [bar], and for [bar]
   to not be deleted even though there are no calls to it. Creating such
   a situation is difficult, and the fact that the following code does so
   is very fragile. This means two things:

     1. This code only tests the appropriate property on amd64
        architectures. Since the code conversion from flambda to
        clambda is architecture independent, this should be fine
        as long as the test is run on such an architecture as part
        of CI.

    2. It is likely that future changes to flambda will silently cause
       this test to stop testing the desired property. It would be worth
       periodically examining the flambda output for the code to check
       that this test is still worth using.
*)

let main x =
  let[@inline never] inner () =
    let[@inline never] foo y () () () () () () () = x + y in
    let x1, x2, x3 = x + 1, x + 2, x + 3 in
    let bar p y () () () =
      if p then foo y () () () () () () ()
      else x1 + x2 + x3
    in
    let[@inline never] baz0 y () () () () () () () =
      let y1 = y + 1 in
      let[@inline never] baz1 () () () () () =
        bar false y1 () () ()
      in
      baz1 () () () () ()
    in
    baz0 1 () () () () () () ()
  in
  inner ()
