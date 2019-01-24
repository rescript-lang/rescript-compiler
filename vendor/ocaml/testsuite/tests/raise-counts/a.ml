let _unused _ = try () with _ -> ()

let trigger_bug x =
  let ok =
    match x with
    | None
    | Some "" -> true
    | Some _ -> false
  in
  if x = Some "" && not ok then
    failwith "impossible"
[@@inline always]
