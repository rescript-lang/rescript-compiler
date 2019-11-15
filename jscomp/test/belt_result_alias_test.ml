(* Make sure Belt.Result.t / Pervasives.result can be used interchangiably *)

let _ =
  Ok "Test"
  |. Belt.Result.map (fun r -> "Value: " ^ r)
  |. Js.log

let _ =
  Belt.Result.(
    Error "error"
    |. map (fun r -> "Value: " ^ r)
    |. getWithDefault("success")
  );
