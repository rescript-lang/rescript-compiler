let test_raises_invalid_argument f x =
  ignore
    (Testing.test_raises_exc_p (function Invalid_argument _ -> true | _ -> false)
         f x)

let check b offset s =
  let rec loop i =
    i = String.length s ||
    Bytes.get b (i + offset) = String.get s i && loop (i+1)
  in
  loop 0

let () =
  let abcde = Bytes.of_string "abcde" in
  let open Bytes in
  begin
    (*
           abcde
    ?????     
    *)
    Testing.test
      (length (extend abcde 7 (-7)) = 5); 

    (*
    abcde
           ?????
    *)
    Testing.test
      (length (extend abcde (-7) 7) = 5);

    (*
      abcde
      abcde
    *)
    Testing.test
      (let r = extend abcde 0 0 in
       length r = 5 && check r 0 "abcde"
       && r != abcde);

    (*
      abcde
    ??abc
    *)
    Testing.test
      (let r = extend abcde 2 (-2) in
       length r = 5 && check r 2 "abc");

    (*
      abcde
       bcd
    *)
    Testing.test
      (let r = extend abcde (-1) (-1) in
       length r = 3 && check r 0 "bcd");

    (*
      abcde
         de??
    *)
    Testing.test
      (let r = extend abcde (-3) 2 in
       length r = 4 && check r 0 "de");

    (*
      abcde
      abc
    *)
    Testing.test
      (let r = extend abcde 0 (-2) in
       length r = 3 && check r 0 "abc");

    (*
      abcde
        cde
    *)
    Testing.test
      (let r = extend abcde (-2) 0 in
       length r = 3 && check r 0 "cde");

    (*
      abcde
      abcde??
    *)
    Testing.test
      (let r = extend abcde 0 2 in
       length r = 7
       && check r 0 "abcde");

    (*
        abcde
      ??abcde
    *)
    Testing.test
      (let r = extend abcde 2 0 in
       length r = 7
       && check r 2 "abcde");

    (*
       abcde
      ?abcde?
    *)
    Testing.test
      (let r = extend abcde 1 1 in
       length r = 7
       && check r 1 "abcde");

    (* length + left + right < 0 *)
    test_raises_invalid_argument
      (fun () -> extend abcde (-3) (-3)) ();

    (* length + left > max_int *)
    test_raises_invalid_argument
      (fun () -> extend abcde max_int 0) ();

    (* length + right > max_int *)
    test_raises_invalid_argument
      (fun () -> extend abcde 0 max_int) ();

    (* length + left + right > max_int *)
    test_raises_invalid_argument
      (fun () -> extend abcde max_int max_int) ();
  end
