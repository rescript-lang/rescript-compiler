(*
  Test that multiple handlers coexist happily.
*)

let test_multiple_handlers =
  let trace = ref [] in
  let collect v = trace := v :: !trace in
  let _ =
    match
      begin 
        match 
          begin
            collect "one";
            failwith "two"
          end
        with
          () -> collect "failure one"
        | exception (Failure x) ->
          collect x;
          failwith "three" 
      end
    with
      () -> 
        collect "failure two";
    | exception (Failure x) ->
      collect x;
      match 
        begin
          collect "four";
          failwith "five"
        end
      with
        () -> collect "failure three"
      | exception (Failure x) ->
        collect x
  in
  print_endline (String.concat " " !trace);
  assert (!trace = [
    "five";
    "four";
    "three";
    "two";
    "one";
  ])
;;
