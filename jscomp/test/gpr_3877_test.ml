let httpResponseCode = 201
let _ =
  match httpResponseCode with
  | 201|200 ->
      Js.log (("good response"))
  | 500|503|506|509|598|501|504|507|510|599|502|505|508|511 ->
      Js.log (("bad response"))
  | _ -> Js.log (("the catch all"))